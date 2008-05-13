module Hyena.Logging
    ( -- * The Logger and LogRequest types
      AccessLogger,
      LogRequest(..),
      ErrorLogger,

      -- * Logging
      startAccessLogger,
      stopAccessLogger,
      logAccess,
      startErrorLogger,
      stopErrorLogger,
      logError,
    ) where

import qualified Data.ByteString.Char8 as C
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Network.Socket (HostAddress, inet_ntoa)
import Network.Wai (Method(..))
import Prelude hiding (log)
import System.IO (Handle, hFlush, hPutStr, hPutStrLn)
import Text.Printf (printf)

import Hyena.Http (Request(..), Response(..))

-- ---------------------------------------------------------------------
-- The Logger and LogRequest types

-- | A queue of messages waiting to be logged.
data Logger a = Logger
    { channel  :: Chan (Maybe a)
    , finished :: MVar ()
    -- ^ The logger puts a value here once it has terminated.
    }

-- | A description of a processed request.
data LogRequest = LogRequest
    { hostAddress :: HostAddress
    , request     :: Request
    , response    :: Response
    }

-- | A logger for client requests.
newtype AccessLogger = AccessLogger (Logger LogRequest)

-- | A logger for error messages.
newtype ErrorLogger = ErrorLogger (Logger String)

-- ---------------------------------------------------------------------
-- Logging

-- | Start a new logger in a separate thread that runs until
-- 'stopLogger' is called.  Returns a 'Logger' that can be used to log
-- messages.
startLogger :: (Handle -> a -> IO ()) -> Handle -> IO (Logger a)
startLogger writer logHandle = do
  chan <- newChan
  finished' <- newEmptyMVar
  forkIO $ logMessages chan finished'
  return $ Logger { channel  = chan
                  , finished = finished'
                  }
    where
      logMessages chan finished' = do
        msg <- readChan chan
        case msg of
          Just msg' -> writer logHandle msg' >>
                          logMessages chan finished'
          Nothing   -> putMVar finished' ()

-- | Stop the access after all currently enqueued log requests have
-- been processed.  Waits until the logger has finished.
stopLogger :: Logger a -> IO ()
stopLogger logger = do
  writeChan (channel logger) Nothing
  takeMVar (finished logger)

-- | Start a new logger that logs client requests.
startAccessLogger :: Handle -> IO AccessLogger
startAccessLogger = fmap AccessLogger . startLogger writeAccess

-- | Stop a client request logger.
stopAccessLogger :: AccessLogger -> IO ()
stopAccessLogger (AccessLogger logger) = stopLogger logger

-- | Start a new logger that logs error messages.
startErrorLogger :: Handle -> IO ErrorLogger
startErrorLogger = fmap ErrorLogger . startLogger writeError

-- | Stop error message logger.
stopErrorLogger :: ErrorLogger -> IO ()
stopErrorLogger (ErrorLogger logger) = stopLogger logger

-- | Log an error.
logError :: ErrorLogger -> String -> IO ()
logError (ErrorLogger logger) msg = writeChan (channel logger) $ Just msg

-- | Write error message to the given 'Handle'.
writeError :: Handle -> String -> IO ()
writeError handle msg = hPutStr handle msg >> hFlush handle

-- | Log a client request.
logAccess :: AccessLogger -> Request -> Response -> HostAddress -> IO ()
logAccess (AccessLogger logger) req resp haddr =
    writeChan (channel logger) $ Just $
              LogRequest
              { hostAddress = haddr
              , request     = req
              , response    = resp
              }

-- | Write client request log message to the given 'Handle'.
writeAccess :: Handle -> LogRequest -> IO ()
writeAccess h logReq = do
  host <- inet_ntoa (hostAddress logReq)
  let requestLine = printf "\"%s %s HTTP/%s\"" method' uri version
      response'   = show (statusCode $ response logReq) ++ " " ++ show length'
  hPutStrLn h $ host ++ " " ++ requestLine ++ " " ++ response'
    where
      (major, minor) = httpVersion $ request logReq
      version        = show major ++ "." ++ show minor
      method'        = prettyPrint $ method $ request logReq
      uri            = C.unpack $ requestUri $ request logReq
      respHeaders = responseHeaders $ response logReq
      -- TODO: Calculate the size in case Content-Length is missing.
      length'        :: Int
      length'        = maybe 0 (read . C.unpack)
                       (lookup (C.pack "Content-Length") respHeaders)

class PrettyPrint a where
    prettyPrint :: a -> String

-- | Converts from a 'Method' enumeration to the corresponding HTTP
-- string.
instance PrettyPrint Method where
    prettyPrint Options = "OPTIONS"
    prettyPrint Get     = "GET"
    prettyPrint Head    = "HEAD"
    prettyPrint Post    = "POST"
    prettyPrint Put     = "PUT"
    prettyPrint Delete  = "DELETE"
    prettyPrint Trace   = "TRACE"
    prettyPrint Connect = "CONNECT"
