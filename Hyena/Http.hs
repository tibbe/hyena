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

      -- * Sending and receiving
      sendResponse,
      isValidStatusCode,
      receiveRequest,

      -- * Common responses
      errorResponse,

      -- * Parsing
      parseRequest
    ) where

import Control.Monad (forM_)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C (map, pack, unpack)
import Data.Char (chr, digitToInt, isAlpha, isDigit, isSpace, ord, toLower)
import Data.Either (either)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Network.Socket (Socket)
import Network.Socket.ByteString (send)

import Hyena.Application (Enumerator, Headers, Method(..))
import Hyena.BufferedSocket
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
    , responseHeaders :: [(S.ByteString, S.ByteString)]
    , responseBody    :: Enumerator
    }

-- ---------------------------------------------------------------------
-- Sending and receiving

-- | Maximum number of bytes sent or received in every socket operation.
blockSize :: Int
blockSize = 4 * 1024

-- | Send response over socket.
sendResponse :: Socket -> Response -> IO ()
sendResponse sock resp = do
  let reasonPhrase = fromJust $ M.lookup (statusCode resp) reasonPhrases
  -- TODO: Check if all data was sent.
  send sock $ (C.pack $ "HTTP/1.1 ")
  send sock $ (C.pack $ show (statusCode resp) ++ " "
                      ++ (C.unpack $ reasonPhrase))
  send sock $ C.pack "\r\n"
  sendHeaders sock (responseHeaders resp)
  send sock $ C.pack "\r\n"
  (responseBody resp) (sendMessageBody sock) ()
  -- TODO: Flush the socket.

-- TODO: Check if all bytes were sent, otherwise retry.

-- | Iteratee used for sending message body over socket.
sendMessageBody :: Socket -> () -> S.ByteString -> IO (Either() ())
sendMessageBody sock _ bs = send sock bs >> return (Right ())

-- | Send headers over socket.
sendHeaders :: Socket -> Headers -> IO ()
sendHeaders sock headers =
    forM_ headers $ \(k, v) -> do
      send sock k
      send sock $ C.pack ": "
      send sock v
      send sock $ C.pack "\r\n"

-- | Receive request from socket.  If the request is malformed
-- 'Nothing' is returned.
receiveRequest :: Socket -> IO (Maybe Request)
receiveRequest sock = do
  bsock <- fromSocket sock
  x <- parseIRequest bsock
  case x of
    Nothing  -> return Nothing
    Just req ->
        return $ do
           len <- contentLength req
           return $
                  Request
                  { method         = iMethod req
                  , requestUri     = iRequestUri req
                  , httpVersion    = iHttpVersion req
                  , requestHeaders = iRequestHeaders req
                  , requestBody    = toEnumerator bsock len
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
      mapFst f = map (\(k, v) -> (f k, v))
      headers = mapFst (C.map toLower) (iRequestHeaders req)

-- ---------------------------------------------------------------------
-- Common responses

-- | An error response with an empty message body.  Closes the
-- connection.
errorResponse :: Int -> Response
errorResponse status =
    Response
    { statusCode      = status
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
digit = satisfy (isDigit . chr . fromIntegral)
letter = satisfy (isAlpha . chr . fromIntegral)
nonSpace = satisfy (not . isSpace . chr . fromIntegral)
notEOL = noneOf $ map c2w "\r\n"

-- | Parser for request \"\r\n\" sequence.
crlf :: Parser S.ByteString
crlf = bytes $ C.pack "\r\n"

-- | Parser that recognize if the current byte is an element of the
-- given sequence of bytes.
oneOf, noneOf :: [Word8] -> Parser Word8
oneOf bs = satisfy (`elem` bs)
noneOf bs = satisfy (`notElem` bs)

-- | Parser for zero or more spaces.
spaces :: Parser [Word8]
spaces = many sp

-- | Parses the header part (i.e. everything expect for the request
-- body) of an HTTP request.  Returns any bytes read that were not
-- used when parsing.  Returns @Nothing@ on failure and @Just
-- (request, remaining)@ on success.
parseIRequest :: BufferedSocket -> IO (Maybe IRequest)
parseIRequest bsock = do
  initial <- readBlock bsock blockSize
  go $ runParser pIRequest initial
    where
      go (Finished req bs) = do putBackBlock bsock bs
                                return $ Just req
      go (Failed _)        = return Nothing
      -- TODO: Detect end of input.
      go (Partial k)       = readBlock bsock blockSize >>= go . k . Just

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
      fieldName = liftA2 (S.cons) letter fieldChars
      contents = liftA2 (S.append) (fmap S.pack $ some notEOL <* crlf)
                 (continuation <|> pure S.empty)
      continuation = liftA2 (S.cons) ((c2w ' ') <$
                                      some (oneOf (map c2w " \t"))) contents

-- It's important that all these three definitions are kept on the top
-- level to have RULES fire correctly.

-- | Parser for zero or more bytes in a header field.
fieldChars :: Parser S.ByteString
fieldChars = fmap S.pack $ many fieldChar

-- fieldChar = letter <|> digit <|> oneOf (map c2w "-_")

-- | Parser for one header field byte.
fieldChar :: Parser Word8
fieldChar = satisfy isFieldChar
    where
      isFieldChar b = (isDigit $ chr $ fromIntegral b) ||
                      (isAlpha $ chr $ fromIntegral b) ||
                      (b `elem` map c2w "-_")

-- ---------------------------------------------------------------------
-- Helpers

-- | An empty 'Enumerator' that just returns the passed in seed.
emptyMessageBody :: Enumerator
emptyMessageBody _ z = return z

-- | Test if the given HTTP status code is valid.
isValidStatusCode :: Int -> Bool
isValidStatusCode code = M.member code reasonPhrases

-- | Mapping from status code to reason phrases.
reasonPhrases :: M.Map Int S.ByteString
reasonPhrases = M.fromList . map (\(k, v) -> (k, C.pack v)) $
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
                     , requestBody    = \f z -> either id id `fmap` (f z bs)
                     }
         in return $ Just (req', S.empty)
