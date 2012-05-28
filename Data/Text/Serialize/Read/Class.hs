{-# LANGUAGE BangPatterns, DefaultSignatures, FlexibleContexts, OverloadedStrings, GeneralizedNewtypeDeriving #-}
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
  readPrec = \n -> to <$> greadPrec n
  {-# INLINE readPrec #-}

class GRead f where
  greadPrec :: ParserPrec (f x)

----------------------------------------------------------------------------------------------------
-- ParserPrec and friends

-- | An attoparsec 'Parser' together with parenthesis information.
type ParserPrec a = Int -> Parser a

atto :: Parser a -> ParserPrec a
atto p = const p
{-# INLINE atto #-}

-- | Consumes all whitespace and parens before p, and just the matching parens after p.
parens_ :: ParserPrec a -> ParserPrec a
parens_ p n = do
  np <- openParens_
  a <- p (if np > 0 then 0 else n)
  closeParens np
  return a
{-# INLINE parens_ #-}

-- | Consumes all the open parens and whitespace, returning how many open parens there were.
openParens_ :: Parser Int
openParens_ = skipSpace *> go 0
  where
    go !acc = (char '(' *> skipSpace *> go (acc + 1)) 
              <|> return acc

closeParens :: Int -> Parser ()
closeParens 0 = return ()
closeParens n = skipSpace *> char ')' *> closeParens (n-1)

prec :: Int -> ParserPrec a -> ParserPrec a
prec n p n' = if n' <= n then p n else empty
{-# INLINE prec #-}
