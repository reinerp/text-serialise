{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Data.Text.Serialize.Show(
  Show(..), 
  showLazyText, 
  -- * Functions for custom 'Show' instances
  showParen, 
  buildPrec, 
  preludeShowPrec,
  defaultShowPrefix,
  ) where

import Data.Text.Serialize.Common
import Data.Text.Serialize.Show.Class(Show(..))
import Data.Text.Serialize.Show.Generic()

import GHC.Generics

import qualified Prelude
import Prelude hiding(Show, show, showsPrec, showParen, showList)
import qualified Data.Text.Buildable as B
import Data.Int
import Data.Char(ord, isDigit)
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import qualified Data.Double.Conversion.Text as C
import Data.Text.Lazy.Builder(Builder, toLazyText)
import Data.Array
import Data.Monoid
import Data.Word

-- just for instances
import qualified GHC.Generics
import qualified Data.Typeable

showLazyText :: Show a => a -> L.Text
showLazyText = toLazyText . show
{-# INLINABLE showLazyText #-}

showParen :: Bool -> Builder -> Builder
showParen !b !p = (if b then "(" else mempty) <> p <> (if b then ")" else mempty)
{-# INLINABLE showParen #-}

buildPrec :: B.Buildable a => Prec -> a -> Builder
buildPrec _prec a = B.build a
{-# INLINABLE buildPrec #-}

numericBuildPrec :: (Ord a, Num a, B.Buildable a) => Prec -> a -> Builder
numericBuildPrec prec a = showParen (prec > minusPrec && a < 0) (B.build a)
{-# INLINABLE numericBuildPrec #-}

preludeShowPrec :: Prelude.Show a => Prec -> a -> Builder
preludeShowPrec n a = B.build (Prelude.showsPrec n a "")
{-# INLINABLE preludeShowPrec #-}

defaultShowPrefix :: Show a => a -> Builder
defaultShowPrefix = showPrec 11
{-# INLINABLE defaultShowPrefix #-}

-- Numeric instances (from Data.Text.Buildable)
instance Show Int     where showPrec = numericBuildPrec; showPrefix = defaultShowPrefix
instance Show Int8    where showPrec = numericBuildPrec; showPrefix = defaultShowPrefix
instance Show Int16   where showPrec = numericBuildPrec; showPrefix = defaultShowPrefix
instance Show Int32   where showPrec = numericBuildPrec; showPrefix = defaultShowPrefix
instance Show Int64   where showPrec = numericBuildPrec; showPrefix = defaultShowPrefix
instance Show Integer where showPrec = numericBuildPrec; showPrefix = defaultShowPrefix
instance Show Word    where showPrec = numericBuildPrec; showPrefix = defaultShowPrefix
instance Show Word8   where showPrec = numericBuildPrec; showPrefix = defaultShowPrefix
instance Show Word16  where showPrec = numericBuildPrec; showPrefix = defaultShowPrefix
instance Show Word32  where showPrec = numericBuildPrec; showPrefix = defaultShowPrefix
instance Show Word64  where showPrec = numericBuildPrec; showPrefix = defaultShowPrefix

-- Float instances (from Data.Double.Conversion.Text)
instance Show Double where 
  showPrec prec a = showParen (prec > minusPrec && a < 0) (B.build . C.toShortest $ a)
  showPrefix = defaultShowPrefix
instance Show Float where 
  showPrec prec a = showParen (prec > minusPrec && a < 0) (B.build . C.toShortest . realToFrac $ a)
  showPrefix = defaultShowPrefix

-- Custom string-like instances
instance Show Char where
  showPrec _ '\'' = "'\\''"
  showPrec _ c = "\'" <> litChar c <> "\'"
  {-# INLINABLE showPrec #-}
  showList str = "\"" <> litText (L.pack str) <> "\""
  {-# INLINABLE showList #-}
  showPrefix = defaultShowPrefix
  {-# INLINABLE showPrefix #-}
instance Show L.Text where
  showPrec _ t = "\"" <> litText t <> "\""
  {-# INLINABLE showPrec #-}
  showPrefix = defaultShowPrefix
  {-# INLINABLE showPrefix #-}
instance Show S.Text where
  showPrec _ t = "\"" <> litText (L.fromStrict t) <> "\""
  {-# INLINABLE showPrec #-}
  showPrefix = defaultShowPrefix
  {-# INLINABLE showPrefix #-}
instance Show SB.ByteString where
  showPrec _ b = "\"" <> litText (L.pack (SB.unpack b)) <> "\""
  {-# INLINABLE showPrec #-}
  showPrefix = defaultShowPrefix
  {-# INLINABLE showPrefix #-}
instance Show LB.ByteString where
  showPrec _ b = "\"" <> litText (L.pack (LB.unpack b)) <> "\""
  {-# INLINABLE showPrec #-}
  showPrefix = defaultShowPrefix
  {-# INLINABLE showPrefix #-}

-- List instance
instance Show a => Show [a] where
  showPrec _ as = showList as
  {-# INLINABLE showPrec #-}
  showPrefix = defaultShowPrefix
  {-# INLINABLE showPrefix #-}

-- Escaping code (for Char and Text)
litText :: L.Text -> Builder
litText t =
  case L.break isEscape t of
    (l, r) -> case L.uncons r of
      Nothing -> B.build l
      Just (c, r') -> case L.uncons r' of
        Nothing -> B.build l <> escape c
        Just (c', _) -> if needsEmptyChar c c'
                        then B.build l <> escape c <> "\\&" <> litText r'
                        else B.build l <> escape c <> litText r'
  where
    isEscape :: Char -> Bool
    isEscape c = c == '\\' || c == '"' || c < ' ' || c >= '\DEL'

    escape :: Char -> Builder
    escape '\\' = "\\\\"
    escape '"' = "\\\""
    escape '\DEL' = "\\DEL"
    escape c | c > '\DEL' = "\\" <> B.build (ord c)
             | otherwise = "\\" <> B.build (escapes ! ord c)

    needsEmptyChar :: Char -> Char -> Bool
    needsEmptyChar '\SOH' 'H' = True
    needsEmptyChar c d = c > '\DEL' && isDigit d

-- Generated by:
-- @map (init . drop 2 . show . chr) [0..ord ' '-1]@
escapes :: Array Int S.Text
escapes = listArray (0,ord ' ')
          ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","a","b","t","n","v","f","r","SO","SI","DLE",
           "DC1","DC2","DC3","DC4","NAK","SYN","ETB","CAN","EM","SUB","ESC","FS","GS","RS","US"]

litChar :: Char -> Builder
litChar '\\' = "\\\\"
litChar '\'' = "\\'"
litChar '\DEL' = "\\DEL"
litChar c | c > '\DEL' = "\\" <> B.build (ord c)
          | c >= ' ' = B.build c
          | otherwise = "\\" <> B.build (escapes ! ord c)

-- Custom tuple instances
instance Show () where showPrec _ () = "()"
instance (Show a, Show b) => Show (a, b) where
  showPrec _ (a, b) = "(" <> show a <> "," <> show b <> ")"
  {-# INLINABLE showPrec #-}
  showPrefix = defaultShowPrefix
  {-# INLINABLE showPrefix #-}
instance (Show a, Show b, Show c) => Show (a, b, c) where
  showPrec _ (a, b, c) = "(" <> show a <> "," <> show b <> "," <> show c <> ")"
  {-# INLINABLE showPrec #-}
  showPrefix = defaultShowPrefix
  {-# INLINABLE showPrefix #-}
instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d) where
  showPrec _ (a, b, c, d) = "(" <> show a <> "," <> show b <> "," <> show c <> "," <> show d <> ")"
  {-# INLINABLE showPrec #-}
  showPrefix = defaultShowPrefix
  {-# INLINABLE showPrefix #-}
instance (Show a, Show b, Show c, Show d, Show e) => Show (a, b, c, d, e) where
  showPrec _ (a, b, c, d, e) = "(" <> show a <> "," <> show b <> "," <> show c <> "," <> show d <> "," <> show e <> ")"
  {-# INLINABLE showPrec #-}
  showPrefix = defaultShowPrefix
  {-# INLINABLE showPrefix #-}
instance (Show a, Show b, Show c, Show d, Show e, Show f) => Show (a, b, c, d, e, f) where
  showPrec _ (a, b, c, d, e, f) = "(" <> show a <> "," <> show b <> "," <> show c <> "," <> show d <> "," <> show e <> "," <> show f <> ")"
  {-# INLINABLE showPrec #-}
  showPrefix = defaultShowPrefix
  {-# INLINABLE showPrefix #-}
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g) => Show (a, b, c, d, e, f, g) where
  showPrec _ (a, b, c, d, e, f, g) = "(" <> show a <> "," <> show b <> "," <> show c <> "," <> show d <> "," <> show e <> "," <> show f <> "," <> show g <> ")"
  {-# INLINABLE showPrec #-}
  showPrefix = defaultShowPrefix
  {-# INLINABLE showPrefix #-}
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h) => Show (a, b, c, d, e, f, g, h) where
  showPrec _ (a, b, c, d, e, f, g, h) = "(" <> show a <> "," <> show b <> "," <> show c <> "," <> show d <> "," <> show e <> "," <> show f <> "," <> show g <> "," <> show h <> ")"
  {-# INLINABLE showPrec #-}
  showPrefix = defaultShowPrefix
  {-# INLINABLE showPrefix #-}
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i) => Show (a, b, c, d, e, f, g, h, i) where
  showPrec _ (a, b, c, d, e, f, g, h, i) = "(" <> show a <> "," <> show b <> "," <> show c <> "," <> show d <> "," <> show e <> "," <> show f <> "," <> show g <> "," <> show h <> "," <> show i <> ")"
  {-# INLINABLE showPrec #-}
  showPrefix = defaultShowPrefix
  {-# INLINABLE showPrefix #-}
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j) => Show (a, b, c, d, e, f, g, h, i, j) where
  showPrec _ (a, b, c, d, e, f, g, h, i, j) = "(" <> show a <> "," <> show b <> "," <> show c <> "," <> show d <> "," <> show e <> "," <> show f <> "," <> show g <> "," <> show h <> "," <> show i <> "," <> show j <> ")"
  {-# INLINABLE showPrec #-}
  showPrefix = defaultShowPrefix
  {-# INLINABLE showPrefix #-}

-- Prelude instances
instance Show GHC.Generics.Arity         where showPrec = preludeShowPrec; showPrefix = defaultShowPrefix
instance Show GHC.Generics.Fixity        where showPrec = preludeShowPrec; showPrefix = defaultShowPrefix
instance Show GHC.Generics.Associativity where showPrec = preludeShowPrec; showPrefix = defaultShowPrefix
instance Show Data.Typeable.TyCon        where showPrec = preludeShowPrec; showPrefix = defaultShowPrefix
instance Show Data.Typeable.TypeRep      where showPrec = preludeShowPrec; showPrefix = defaultShowPrefix

-- Generic instances
instance Show Bool
instance Show Ordering
instance (Show a, Show b) => Show (Either a b)
