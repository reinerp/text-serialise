{-# LANGUAGE DefaultSignatures, FlexibleContexts, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Data.Text.Serialize.Read.Class where

import Control.Monad.Reader
import Control.Monad
import Data.Attoparsec.Text
import GHC.Generics
import Control.Applicative
import Prelude hiding (Read(..))
import Data.Text.Serialize.Read.Lex
import qualified Data.Text as T

class Read a where
  readPrec :: ParserPrec a

  default readPrec :: (Generic a, GRead (Rep a)) => ParserPrec a
  {-# INLINE readPrec #-}
  readPrec = \n -> to <$> greadPrec n

class GRead f where
  greadPrec :: ParserPrec (f x)

----------------------------------------------------------------------------------------------------
-- ParserPrec and friends

-- | An attoparsec 'Parser' together with parenthesis information.
type ParserPrec a = Int -> Parser a

{-# INLINE atto #-}
atto :: Parser a -> ParserPrec a
atto p = const p

{-# INLINE parens #-}
parens :: ParserPrec a -> ParserPrec a
parens p = optional
  where
    optional n = p n <|> mandatory n
    mandatory = paren optional

{-# INLINE paren #-}
paren :: ParserPrec a -> ParserPrec a
paren p n = punc '(' *> p 0 <* punc ')'

{-# INLINE prec #-}
prec :: Int -> ParserPrec a -> ParserPrec a
prec n p n' = if n' <= n then p n else empty
