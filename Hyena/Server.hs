{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, ScopedTypeVariables, RankNTypes #-}

------------------------------------------------------------------------
-- |
-- Module      :  Hyena.Server
-- Copyright   :  (c) Johan Tibell 2008
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  johan.tibell@gmail.com
-- Stability   :  experimental
-- Portability :  not portable, uses cunning newtype deriving
--
-- Core module of the server.  Receives HTTP requests, runs the
-- application and sends responses.
--
------------------------------------------------------------------------

module Hyena.Server
    ( serve,
      serveWithConfig
    ) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Exception.Extensible
import Control.Monad (unless, when)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, ask, asks,
                             liftIO, runReaderT)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C (elemIndex, pack)
import Network.BSD (getProtocolNumber)
import Network.Socket (Family(..), HostAddress, SockAddr(..), Socket,
                       SocketOption(..), SocketType(..), accept, bindSocket,
                       listen, inet_addr, maxListenQueue, sClose,
                       setSocketOption, socket, withSocketsDo)
import Network.Wai
import Prelude hiding (catch, log)
import System.Exit (exitFailure, ExitCode(..))
import System.IO (Handle, stderr, hPutStrLn)
#ifndef mingw32_HOST_OS
import System.Posix.Signals (Handler(..), installHandler, sigPIPE)
#endif

import Hyena.Config
import Hyena.Http
import Hyena.Logging

-- TODO: Header names are not case sensitive and header values may or
-- may not be.

-- TODO: How do we handle the fact that an error can be detected in
-- the middle of receiving data using chunked encoding, e.g. the
-- number indicating the chunk size contains non-hex characters.

-- ---------------------------------------------------------------------
-- The Server type

newtype Server a = Server (ReaderT ServerConfig IO a)
    deriving (Monad, MonadIO, MonadReader ServerConfig)

-- | The server configuration.  Includes the user supplied
-- configuration and the loggers used by the server.
data ServerConfig = ServerConfig
    { config       :: Config  -- ^ The initial user configuration.
    , accessLogger :: AccessLogger  -- ^ Access logger.
    , errorLogger  :: ErrorLogger  -- ^ Error logger.
    }

-- | Run the server action.
runServer :: ServerConfig -> Server a -> IO a
runServer conf (Server a) = runReaderT a conf

-- | Run action in the server monad, and in case of exception, and
-- catch it and run the error case.
catchServer ::Server a -> (forall e. (Exception e) => e ->Server a) -> Server a
catchServer m k = do
  conf <- ask
  io $ runServer conf m `catches` handlers conf
    where handlers c 
              = [ Handler $ \(e::ExitCode)      ->throw e
                , Handler $ \(e::SomeException) ->runServer c $ k e ]

-- | Run the first action and then the second action.  The second
-- action is run even if the first action threw and exception.
finallyServer :: Server a -> Server b -> Server a
finallyServer m k = do
  conf <- ask
  io $ runServer conf m `finally` runServer conf k

-- | Fork a new thread.
forkServer :: Server () -> Server ThreadId
forkServer m = do
  conf <- ask
  io $ forkIO $ runServer conf m

-- ---------------------------------------------------------------------
-- Running applications

-- | Forward requests to the given 'Application' forever.  Read server
-- configuration from command line flags.
serve :: Application -> IO ()
serve application = do
  conf <- configFromFlags
  serveWithConfig conf application

-- TODO: Fork a new daemonized thread/process if daemonized mode is
-- requested.  This is currently not possible because of limitations
-- in the GHC RTS.

-- | Forward requests to the given 'Application' forever.  Use
-- supplied server configuration.
serveWithConfig :: Config -> Application -> IO ()
serveWithConfig conf application = do
#ifndef mingw32_HOST_OS
  installHandler sigPIPE Ignore Nothing
#endif
  when (daemonize conf) $ do
    hPutStrLn stderr "Daemonized mode not supported at the moment."
    hPutStrLn stderr $ "If you need this feature please say so in " ++
                  "GHC ticket #1185."
    exitFailure
  bracketLoggers (logHandle conf) $ \accessLog errorLog ->
      let serverConf = ServerConfig
                       { config       = conf
                       , accessLogger = accessLog
                       , errorLogger  = errorLog
                       }
      in runServer serverConf $ serve' application

-- ---------------------------------------------------------------------
-- Networking

-- | Start loggers, run an action using those loggers and when
-- finished, stop the loggers.
bracketLoggers :: Handle -> (AccessLogger -> ErrorLogger -> IO ()) -> IO ()
bracketLoggers h =
    bracket (do accessLog <- startAccessLogger h
                errorLog <- startErrorLogger stderr
                return (accessLog, errorLog))
                (\(accessLog, errorLog) -> do
                   stopErrorLogger errorLog
                   stopAccessLogger accessLog)
                . uncurry

-- | Open the server socket and start accepting connections.
serve' :: Application -> Server ()
serve' application = do
  conf <- ask
  port' <- asks (fromIntegral . port . config)
  address' <- asks (address . config)
  io $ withSocketsDo $
     do proto <- getProtocolNumber "tcp"
        addr <- inet_addr address'
        bracket (socket AF_INET Stream proto)
                 sClose
                 (\sock -> do
                    setSocketOption sock ReuseAddr 1
                    bindSocket sock (SockAddrInet port' addr)
                    listen sock maxListenQueue
                    runServer conf $ acceptConnections application sock)

-- | Accept connections, and fork off a new thread to handle each one.
acceptConnections :: Application -> Socket -> Server ()
acceptConnections application serverSock = do
  (sock, SockAddrInet _ haddr) <- io $ accept serverSock
  forkServer ((talk sock haddr application `finallyServer`
               (io $ sClose sock))
              `catchServer`
              (\e -> do logger <- asks errorLogger
                        io $ logError logger $ show e))
  acceptConnections application serverSock

-- | Read the client input, parse the request, run the application,
-- and send a response.
talk :: Socket -> HostAddress -> Application -> Server ()
talk sock haddr application = do
  req <- io $ receiveRequest sock
  case req of
    Nothing  -> io $ sendResponse sock $ errorResponse 400
    Just req' ->
        -- TODO: Validate the request:
        --    * If HTTP 1.1 Host MUST be present.
        do errorLogger' <- asks errorLogger
           let environ = requestToEnvironment (logError errorLogger') req'
           resp <- run environ application
           accessLogger' <- asks accessLogger
           io $ logAccess accessLogger' req' resp haddr
           io $ sendResponse sock resp
           unless (closeConnection req' resp) $
                  talk sock haddr application

-- | Run the application and send a response.
run :: Environment -> Application -> Server Response
run environ application = io $ do
  -- TODO: Check the validity of the returned status code and headers
  -- and log an error and send a 500 if either is invalid.
  (status, reason, headers', output) <- application environ
  return Response
           { statusCode      = status
           , reasonPhrase    = reason
           , responseHeaders = headers'
           , responseBody    = output
           }

-- | Check if the connection should be closed after processing this
-- request.
closeConnection :: Request -> Response -> Bool
closeConnection req resp =
    let reqHdr       = lookup hdrName (requestHeaders req)
        respHdr      = lookup hdrName (responseHeaders resp)
        closeSet     =
            case (reqHdr, respHdr) of
              (Just v, _) | v == closeVal -> True
              (_, Just v) | v == closeVal -> True
              _                           -> False
        keepAliveSet =
            case reqHdr of
              Just v | v == keepAliveVal -> True
              _                          -> False
    in closeSet || (httpVersion req < (1,1) && not keepAliveSet)
    where
      hdrName      = C.pack "Connection"
      closeVal     = C.pack "close"
      keepAliveVal = C.pack "keep-alive"

-- ---------------------------------------------------------------------
-- General utilities

-- | Lift an IO action into the 'Server' monad.
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Convert an HTTP 'Request' to an 'Environment'.
requestToEnvironment :: (String -> IO ()) -> Request -> Environment
requestToEnvironment err req =
    Environment
    { requestMethod   = method req
    , scriptName      = S.empty
    , pathInfo        = path
    , queryString     = query
    , requestProtocol = httpVersion req
    , headers         = requestHeaders req
    , input           = requestBody req
    , errors          = err
    }
    where
      (path, query) = splitRequestUri $ requestUri req
      splitRequestUri uri =
          let index = C.elemIndex '?' uri
          in case index of
               Nothing -> (uri, Nothing)
               Just i  -> (S.take i uri, Just $ S.drop (i + 1) uri)
