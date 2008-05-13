module Hyena.BufferedSocket
    ( BufferedSocket,
      fromSocket,
      readBlock,
      putBackBlock,
      toEnumerator
    ) where

import qualified Data.ByteString as S
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv)

import Network.Wai (Enumerator)

data BufferedSocket = BufferedSocket
    { buffer :: IORef S.ByteString
    , socket :: Socket
    }

blockSize :: Int
blockSize = 4 * 1024

fromSocket :: Socket -> IO BufferedSocket
fromSocket sock = do
  buffRef <- newIORef S.empty
  return $ BufferedSocket
             { buffer = buffRef
             , socket = sock
             }

readBlock :: BufferedSocket -> Int -> IO S.ByteString
readBlock bsock n = do
  buf <- readIORef $ buffer bsock
  if S.null buf
     then do
       bs <- recv (socket bsock) blockSize
       split bs
     else split buf
    where
      split bs
          | n < S.length bs = do
                      let (bs', rest) = S.splitAt n bs
                      writeIORef (buffer bsock) rest
                      return bs'
          | otherwise       = return bs

putBackBlock :: BufferedSocket -> S.ByteString -> IO ()
putBackBlock bsock bs = do
  modifyIORef (buffer bsock) (flip S.append bs)

toEnumerator :: BufferedSocket -> Int -> Enumerator
toEnumerator bsock maxBytes = go maxBytes
    where
      go 0 _ z = return z
      go n f z = do
        bs <- readBlock bsock n
        if S.null bs
           then return z
           else do
             z' <- f z bs
             case z' of
               Left z''  -> return z''
               Right z'' -> go (n - S.length bs) f z''
