{-# LANGUAGE Rank2Types #-}

------------------------------------------------------------------------
-- |
-- Module      :  Hyena.Parser
-- Copyright   :  (c) Johan Tibell 2008
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  johan.tibell@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- An incremental LL(1) parser combinator library.
--
------------------------------------------------------------------------

module Hyena.Parser
    (
      -- * The Parser type
      Parser,
      Result(..),
      runParser,

      -- * Primitive parsers
      satisfy,
      byte,
      bytes,

      module Control.Applicative
    ) where

import Control.Applicative
import qualified Data.ByteString as S
import Data.Int (Int64)
import Data.Word (Word8)
import Prelude hiding (fail, succ)

-- ---------------------------------------------------------------------
-- The Parser type

-- | The parse state.
data S = S
    {-# UNPACK #-} !S.ByteString
    {-# UNPACK #-} !Int64
    {-# UNPACK #-} !Bool
         deriving Show

-- | A parse either succeeds, fails or returns a suspension with which
-- the parsing can be resumed.
data Result a = Finished a S.ByteString
              -- ^ Parsing succeeded and produced a value of type
              -- @a@. The returned 'S.ByteString' is the remaining
              -- unconsumed input.
              | Failed Int64
              -- ^ Parsing failed at the given position. Either
              -- because the parser didn't match the input or because
              -- an unexpected end of input was reached during
              -- parsing.
              | Partial (Maybe S.ByteString -> Result a)
              -- ^ The parsing needs more input to continue. Pass in
              -- @Just input@ to continue parsing and @Nothing@ to
              -- signal end of input. If @Nothing@ is passed the
              -- 'Result' is either 'Finished' or 'Failed'.

data IResult a = IFinished a S
               | IFailed Int64
               | IPartial (Maybe S.ByteString -> IResult a)

toResult :: IResult a -> Result a
toResult (IFinished a (S bs _ _)) = Finished a bs
toResult (IFailed pos)            = Failed pos
toResult (IPartial k)             = Partial $ toResult . k

-- | A parser takes a parse state, a success continuation and a
-- failure continuation and returns a 'Result'.
newtype Parser a = Parser
    { unParser :: forall r.
                  S -> (a -> S -> IResult r) -> (S -> IResult r) -> IResult r }

-- ---------------------------------------------------------------------
-- Instances

instance Functor Parser where
    fmap f p = Parser $ \s succ fail -> unParser p s (succ . f) fail

instance Applicative Parser where
    pure a = Parser $ \s succ _ -> succ a s
    p <*> p' = Parser $ \s succ fail ->
               flip (unParser p s) fail $ \f s' ->
                   unParser p' s' (succ . f) fail

instance Alternative Parser where
    empty = Parser $ \s _ fail -> fail s
    p <|> p' = Parser $ \s@(S _ pos _) succ fail ->
               unParser p s succ $ \s'@(S _ pos' _) ->
                   if pos == pos'
                   then unParser p' s' succ fail
                   else fail s'

-- ---------------------------------------------------------------------
-- Running a parser

initState :: S.ByteString -> S
initState bs = S bs 0 False
{-# INLINE initState #-}

-- | This is the final continuation that turns a successful parse into
-- a 'IResult'.
finished :: a -> S -> IResult a
finished v s = IFinished v s

-- | This is the final continuation that turns an unsuccessful parse
-- into a 'IResult'.
failed :: S -> IResult a
failed (S _ pos _) = IFailed pos

-- | TODO: Write documentation.
runParser :: Parser a -> S.ByteString -> Result a
runParser p bs = toResult $ unParser p (initState bs) finished failed

-- ---------------------------------------------------------------------
-- Primitive parsers

-- | The parser @satisfy p@ succeeds for any byte for which the
-- supplied function @p@ returns 'True'.  Returns the byte that is
-- actually parsed.
satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy p =
    Parser $ \s@(S bs pos eof) succ fail ->
        case S.uncons bs of
          Just (b, bs') -> if p b
                           then succ b (S bs' (pos + 1) eof)
                           else fail s
          Nothing       -> if eof
                           then fail s
                           else IPartial $ \x ->
                               case x of
                                 Just bs' -> retry (S bs' pos eof)
                                 Nothing  -> fail (S bs pos True)
            where retry s' = unParser (satisfy p) s' succ fail

-- | @byte b@ parses a single byte @b@.  Returns the parsed byte
-- (i.e. @b@).
byte :: Word8 -> Parser Word8
byte b = satisfy (== b)

-- | @bytes bs@ parses a sequence of bytes @bs@.  Returns the parsed
-- bytes (i.e. @bs@).
bytes :: S.ByteString -> Parser S.ByteString
bytes = fmap S.pack . go
    where
      go bs = case S.uncons bs of
                Just (b, bs') -> liftA2 (:) (byte b) (go bs')
                Nothing       -> pure []
