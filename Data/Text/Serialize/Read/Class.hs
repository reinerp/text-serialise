module Data.Text.Serialize.Read.Class where

import Data.Attoparsec.Text
import GHC.Generics
import Control.Applicative
import Prelude hiding (Read(..))
import Data.Text.Serialize.Read.Lex

class Read a where
  readPrec :: ParserP a

  default readPrec :: (Generic a, GRead (Rep a)) => ParserP a
  readPrec n = to <$> greadPrec n

class GRead f where
  greadPrec :: ParserP (a x)

-- | An attoparsec 'Parser' together with parenthesis information.
type ParserP a = Int -> Parser a

parens :: Int -> ParserP a -> ParserP a
parens p = optional
  where
    optional = p 

prec :: Int -> ParserP a -> ParserP a
prec n p = \n' -> if n' <= n 
                  then p n 
                  else empty

