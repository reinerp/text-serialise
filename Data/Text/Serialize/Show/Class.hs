{-# LANGUAGE DefaultSignatures, FlexibleContexts, OverloadedStrings #-}
module Data.Text.Serialize.Show.Class where

import GHC.Generics
import Data.Text.Lazy.Builder
import Data.Monoid((<>))

class Show a where
  showPrec :: Int -- ^ the operator precedence of the enclosing
              -- context (a number from 0 to 11).
              -- Function application has precedence 10.
        -> a -- ^ the value to be shown      
        -> Builder

  default showPrec :: (Generic a, GShow (Rep a)) => Int -> a -> Builder
  showPrec prec a = gshowPrec prec (from a)
  {-# INLINE showPrec #-}

  showList :: [a] -> Builder
  showList [] = "[]"
  showList (x:xs) = "[" <> showPrec 0 x <> foldr (\y rest -> "," <> showPrec 0 y <> rest) "]" xs
  {-# INLINABLE showList #-}

  show :: a -> Builder
  show = showPrec 0
  {-# INLINABLE show #-}

class GShow f where 
  gshowPrec :: Int -> f x -> Builder
