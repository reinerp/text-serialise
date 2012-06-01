module Data.Text.Serialize.Common(
  -- * Precedences
  Prec,
  appPrec,
  minusPrec,
  -- * Misc
  __,
 ) where

type Prec = Int

appPrec :: Prec
appPrec = 10

minusPrec :: Prec
minusPrec = 5

__ :: a
__ = undefined
