{-# LANGUAGE Rank2Types #-}

module Data.Enumerator
    ( -- Enumerators
      bytesEnum,
      chunkEnum,
      partialSocketEnum,
      socketEnum,

      -- Combining enumerators
      compose
    ) where

import Control.Monad (liftM)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C (unpack)
import Data.Word (Word8)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv)
import Numeric (readHex)

type IterateeM a m = a -> S.ByteString -> m (Either a a)
type EnumeratorM m = forall a. IterateeM a m -> a -> m a

-- -----------------------------------------------------------
-- Enumerators

-- | Enumerates a 'ByteString'.
bytesEnum :: Monad m => S.ByteString -> EnumeratorM m
bytesEnum bs f seed = do
  seed' <- f seed bs
  case seed' of
    Left seed''  -> return seed''
    Right seed'' -> return seed''


nl :: Word8
nl = 10

-- | Enumerates chunks of data encoded using HTTP chunked encoding.
chunkEnum :: Monad m => EnumeratorM m -> EnumeratorM m
chunkEnum enum f initSeed = fst `liftM` enum go (initSeed, Left S.empty)
    where
      go (seed, Left acc) bs =
        case S.elemIndex nl bs of
          Just ix -> let (line, rest) = S.splitAt (ix + 1) bs
                         hdr          = S.append acc line
                         chunkLen     = pHeader hdr
                     in case chunkLen of
                          Just n  -> go (seed, Right n) rest
                          Nothing -> error $ "malformed header" ++ show hdr
          Nothing -> return $ Right (seed, Left (S.append acc bs))
      go (seed, Right n) bs  =
        let len = S.length bs
        in if len < n
           then do
             seed' <- f seed bs
             case seed' of
               Right seed'' -> return $ Right (seed'', Right $! n - len)
               Left  seed'' -> return $ Left (seed'', Right $! n - len)
           else let (bs', rest) = S.splitAt n bs
                in do
                  seed' <- f seed bs'
                  case seed' of
                    Right seed'' -> go (seed'', Left S.empty) rest
                    Left  seed'' -> return $ Left (seed'', Left rest)

-- TODO: Ignore header.
pHeader :: S.ByteString -> Maybe Int
pHeader bs =
    case readHex $ C.unpack hdr of
      [(n, "")] -> Just n
      _         -> Nothing
    where
      hdr = S.take (S.length bs - 2) bs

-- | Maximum number of bytes sent or received in every socket
-- operation.
blockSize :: Int
blockSize = 4 * 1024

-- | @partialSocketEnum sock numBytes@ enumerates @numBytes@ bytes
-- received through the given socket.  Does not close the socket.
partialSocketEnum :: Socket -> Int -> EnumeratorM IO
partialSocketEnum sock numBytes f initSeed = go initSeed numBytes
  where
    go seed 0 = return seed
    go seed n = do
      bs <- recv sock blockSize
      if S.null bs
        then return seed
        else do
          seed' <- f seed bs
          case seed' of
            Right seed'' -> go seed'' $! n - S.length bs
            Left seed''  -> return seed''

-- | Enumerates data received through the given socket.  Does not
-- close the socket.
socketEnum :: Socket -> EnumeratorM IO
socketEnum sock f initSeed = go initSeed
  where
    go seed = do
      bs <- recv sock blockSize
      if S.null bs
        then return seed
        else do
          seed' <- f seed bs
          case seed' of
            Right seed'' -> go seed''
            Left seed''  -> return seed''

-- -----------------------------------------------------------
-- Combining enumerators

-- Make two enumerators behave like one.
compose :: Monad m => EnumeratorM m -> EnumeratorM m -> EnumeratorM m
compose enum1 enum2 f initSeed = enum1 f1 (Right initSeed) >>= k
    where
      f1 (Right seed) bs = do
        r <- f seed bs
        case r of
          x@(Right _) -> return $ Right x
          x           -> return $ Left x
      f1 x _              = return $ Left x  -- Cannot happen.
      k (Left seed)  = return seed
      k (Right seed) = enum2 f seed
