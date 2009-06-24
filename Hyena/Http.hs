{-# LANGUAGE Rank2Types  #-}

-- See: http://www.w3.org/Protocols/rfc2616/rfc2616.html

------------------------------------------------------------------------
-- |
-- Module      :  Hyena.Http
-- Copyright   :  (c) Johan Tibell 2008
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  johan.tibell@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Receiving and sending HTTP requests and responses.
--
------------------------------------------------------------------------

module Hyena.Http
    ( -- * The request and response data types
      Request(..),
      Response(..),
      ReceiveResult(..),

      -- * Sending and receiving
      sendResponse,
      isValidStatusCode,
      receiveRequest,

      -- * Common responses
      errorResponse,

      -- * Parsing
      parseRequest
    ) where

import Prelude hiding (catch)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C (map, pack, unpack)
import Data.Char (chr, digitToInt, isAlpha, isDigit, isSpace, ord, toLower)
import Data.Either (either)
import Control.Arrow
import Control.Exception (IOException, catch, throw)
import Control.Monad (when)
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)
import Data.Word (Word8)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)
import Network.Wai (Enumerator, Headers, Method(..))

import Data.Enumerator
import Hyena.Parser

-- ---------------------------------------------------------------------
-- Request and response data types

-- | An HTTP request.
data Request = Request
    { method         :: Method
    , requestUri     :: S.ByteString
    , httpVersion    :: (Int, Int)
    , requestHeaders :: [(S.ByteString, S.ByteString)]
    , requestBody    :: Enumerator
    }

-- | An internal representation of the request header part of an HTTP
-- request.
data IRequest = IRequest
    { iMethod         :: Method
    , iRequestUri     :: S.ByteString
    , iHttpVersion    :: (Int, Int)
    , iRequestHeaders :: [(S.ByteString, S.ByteString)]
    } deriving Show

-- | An HTTP response.
data Response = Response
    { statusCode      :: Int
    , reasonPhrase    :: S.ByteString
    , responseHeaders :: [(S.ByteString, S.ByteString)]
    , responseBody    :: Enumerator
    }

-- | A data structure used to keep track of the result of parsing a
-- request from the client.
data ReceiveResult a = ParseSuccess a
                     | ParseError
                     | ClientDisconnect


-- ---------------------------------------------------------------------
-- Sending and receiving

-- | Maximum number of bytes sent or received in every socket operation.
blockSize :: Int
blockSize = 4 * 1024

-- | Send response over socket.
sendResponse :: Socket -> Response -> IO ()
sendResponse sock resp = do
  sendAll sock $ S.concat [C.pack "HTTP/1.1 "
                          ,(C.pack $ show (statusCode resp) ++ " "
                              ++(C.unpack $ reasonPhrase resp))
                          ,C.pack "\r\n"]
  sendHeaders sock (responseHeaders resp)
  sendAll sock $ C.pack "\r\n"
  r <- responseBody resp (sendMessageBody sock) Nothing
  when (isJust r) $ throw (fromJust r)
  -- TODO: Flush the socket.

-- | Iteratee used for sending message body over socket.
sendMessageBody :: Socket
                -> Maybe IOException
                -> S.ByteString
                -> IO (Either (Maybe IOException) (Maybe IOException))
sendMessageBody sock _ bs =
  catch (sendAll sock bs >> return (Right Nothing))
        (\e -> return $ Left (Just e))

-- | Send headers over socket.
sendHeaders :: Socket -> Headers -> IO ()
sendHeaders sock headers = do
  sendAll sock $ S.concat $ map go headers
  return ()
    where go (k, v) = S.concat [k, C.pack ": "
                               ,v, C.pack "\r\n"]

-- | Receive request from socket.  Returns @ParseError@ on parse
-- failure, @ClientDisconnect@ if the client disconnected
-- unexpectedly, and @ParseSuccess request@ on success.
receiveRequest :: Socket -> IO (ReceiveResult Request)
receiveRequest sock = do
  x <- parseIRequest sock
  case x of
    ClientDisconnect -> return ClientDisconnect
    ParseError       -> return ParseError

    ParseSuccess (req, bs) ->
        let len  = contentLength req
            rest = bytesEnum bs
            enum = case len of
                     Just n  -> partialSocketEnum sock (n - S.length bs)
                     Nothing -> chunkEnum $ socketEnum sock
        in return $ ParseSuccess
           Request
           { method         = iMethod req
           , requestUri     = iRequestUri req
           , httpVersion    = iHttpVersion req
           , requestHeaders = iRequestHeaders req
           , requestBody    = compose rest enum
           }

-- | The length of the request's message body, if present.
contentLength :: IRequest -> Maybe Int
contentLength req
    | iMethod req `elem` [Options, Get, Head] = Just 0
    | otherwise = do v <- getHeader "Content-Length" req
                     toInt $ C.unpack v
    where
      toInt str = case reads str of
                    [(i, "")] -> Just i
                    _         -> Nothing

-- | Get header if present.
getHeader :: String -> IRequest -> Maybe S.ByteString
getHeader hdr req = lookup (C.map toLower $ C.pack hdr) headers
    where
      mapFst  = map . first
      headers = mapFst (C.map toLower) (iRequestHeaders req)

-- ---------------------------------------------------------------------
-- Common responses

-- | An error response with an empty message body.  Closes the
-- connection.
errorResponse :: Int -> Response
errorResponse status =
    Response
    { statusCode      = status
    , reasonPhrase    = fromJust $ M.lookup status reasonPhrases
    , responseHeaders = [(C.pack "Connection", C.pack "close")]
    , responseBody    = emptyMessageBody
    }

-- ---------------------------------------------------------------------
-- Parsing requests

c2w :: Char -> Word8
c2w = fromIntegral . ord

-- | Parsers for different tokens in an HTTP request.
sp, digit, letter, nonSpace, notEOL :: Parser Word8
sp = byte $ c2w ' '
digit = satisfies (isDigit . chr . fromIntegral)
letter = satisfies (isAlpha . chr . fromIntegral)
nonSpace = satisfies (not . isSpace . chr . fromIntegral)
notEOL = noneOf $ map c2w "\r\n"

-- | Parser for request \"\r\n\" sequence.
crlf :: Parser S.ByteString
crlf = bytes $ C.pack "\r\n"

-- | Parser that recognize if the current byte is an element of the
-- given sequence of bytes.
oneOf, noneOf :: [Word8] -> Parser Word8
oneOf bs = satisfies (`elem` bs)
noneOf bs = satisfies (`notElem` bs)

-- | Parser for zero or more spaces.
spaces :: Parser [Word8]
spaces = many sp

-- | Parses the header part (i.e. everything expect for the request
-- body) of an HTTP request.  Returns any bytes read that were not
-- used when parsing.  Returns @ParseError@ on parse failure,
-- @ClientDisconnect@ if the client disconnected unexpectedly, and
-- @ParseSuccess (request, remaining)@ on success.
parseIRequest :: Socket -> IO (ReceiveResult (IRequest, S.ByteString))
parseIRequest sock = do
  initial <- recv sock blockSize
  if S.null initial then
    return ClientDisconnect
    else go $ runParser pIRequest initial
    where
      go (Finished req bs) = return $ ParseSuccess (req, bs)
      go (Failed _)        = return ParseError
      go (Partial k)       = do
        received <- recv sock blockSize
        if S.null received then do
          return ClientDisconnect
          else (go . k . Just) received

-- | Parser for the internal request data type.
pIRequest :: Parser IRequest
pIRequest = IRequest
               <$> pMethod <* sp
               <*> pUri <* sp
               <*> pVersion <* crlf
               <*> pHeaders <* crlf

-- | Parser for the request method.
pMethod :: Parser Method
pMethod = (Options <$ bytes (C.pack "OPTIONS"))
          <|> (Get <$ bytes (C.pack "GET"))
          <|> (Head <$ bytes (C.pack "HEAD"))
          <|> byte (c2w 'P') *> ((Post <$ bytes (C.pack "OST")) <|>
                                 (Put <$ bytes (C.pack "UT")))
          <|> (Delete <$ bytes (C.pack "DELETE"))
          <|> (Trace <$ bytes (C.pack "TRACE"))
          <|> (Connect <$ bytes (C.pack "CONNECT"))

-- | Parser for the request URI.
pUri :: Parser S.ByteString
pUri = fmap S.pack $ many nonSpace

-- | Parser for the request's HTTP protocol version.
pVersion :: Parser (Int, Int)
pVersion = bytes (C.pack "HTTP/") *>
           liftA2 (,) (digit' <* byte (c2w '.')) digit'
    where
      digit' = fmap (digitToInt . chr . fromIntegral) digit

-- | Parser for request headers.
pHeaders :: Parser [(S.ByteString, S.ByteString)]
pHeaders = many header
    where
      header = liftA2 (,) fieldName (byte (c2w ':') *> spaces *> contents)
      fieldName = liftA2 S.cons letter fieldChars
      contents = liftA2 S.append (fmap S.pack $ some notEOL <* crlf)
                 (continuation <|> pure S.empty)
      continuation = liftA2 S.cons (c2w ' ' <$
                                    some (oneOf (map c2w " \t"))) contents

-- It's important that all these three definitions are kept on the top
-- level to have RULES fire correctly.

-- | Parser for zero or more bytes in a header field.
fieldChars :: Parser S.ByteString
fieldChars = fmap S.pack $ many fieldChar

-- fieldChar = letter <|> digit <|> oneOf (map c2w "-_")

-- | Parser for one header field byte.
fieldChar :: Parser Word8
fieldChar = satisfies isFieldChar
    where
      isFieldChar b = (isDigit $ chr $ fromIntegral b) ||
                      (isAlpha $ chr $ fromIntegral b) ||
                      (b `elem` map c2w "-_")

-- ---------------------------------------------------------------------
-- Helpers

-- | An empty 'Enumerator' that just returns the passed in seed.
emptyMessageBody :: Enumerator
emptyMessageBody _ = return

-- | Test if the given HTTP status code is valid.
isValidStatusCode :: Int -> Bool
isValidStatusCode code = M.member code reasonPhrases

-- | Mapping from status code to reason phrases.
reasonPhrases :: M.Map Int S.ByteString
reasonPhrases = M.fromList . map (second C.pack) $
                [(100, "Continue")
                ,(101, "Switching Protocols")
                ,(200, "OK")
                ,(201, "Created")
                ,(202, "Accepted")
                ,(203, "Non-Authoritative Information")
                ,(204, "No Content")
                ,(205, "Reset Content")
                ,(206, "Partial Content")
                ,(300, "Multiple Choices")
                ,(301, "Moved Permanently")
                ,(302, "Found")
                ,(303, "See Other")
                ,(304, "Not Modified")
                ,(305, "Use Proxy")
                ,(307, "Temporary Redirect")
                ,(400, "Bad Request")
                ,(401, "Unauthorized")
                ,(402, "Payment Required")
                ,(403, "Forbidden")
                ,(404, "Not Found")
                ,(405, "Method Not Allowed")
                ,(406, "Not Acceptable")
                ,(407, "Proxy Authentication Required")
                ,(408, "Request Time-out")
                ,(409, "Conflict")
                ,(410, "Gone")
                ,(411, "Length Required")
                ,(412, "Precondition Failed")
                ,(413, "Request Entity Too Large")
                ,(414, "Request-URI Too Large")
                ,(415, "Unsupported Media Type")
                ,(416, "Requested range not satisfiable")
                ,(417, "Expectation Failed")
                ,(500, "Internal Server Error")
                ,(501, "Not Implemented")
                ,(502, "Bad Gateway")
                ,(503, "Service Unavailable")
                ,(504, "Gateway Time-out")
                ,(505, "HTTP Version not supported")]

-- TODO: Return remaining bytes if Content-Length present.

-- | Parse a request from a sequence of bytes.
parseRequest :: S.ByteString -> IO (Maybe (Request, S.ByteString))
parseRequest input =
  go $ runParser pIRequest input
    where
      go (Failed _)        = return Nothing
      go (Partial k)       = go (k Nothing)
      go (Finished req bs) =
          let req' = Request
                     { method         = iMethod req
                     , requestUri     = iRequestUri req
                     , httpVersion    = iHttpVersion req
                     , requestHeaders = iRequestHeaders req
                     , requestBody    = \f z -> either id id `fmap` f z bs
                     }
         in return $ Just (req', S.empty)
