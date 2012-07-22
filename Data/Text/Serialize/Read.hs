module Data.Text.Serialize.Read(
  Read(..),
  read,
  readEither,
  ParserPrec(..),
 ) where

import Prelude hiding (Read(..), read)
import Control.Applicative
import Data.Text.Serialize.Read.Class
import Data.Text.Serialize.Read.Generic()
import Data.Attoparsec.Text(Parser, parseOnly, decimal, rational, skipSpace, char)
import Data.Int
import Data.Word
import Data.Text(Text)

read :: Read a => Text -> a
read t = case readEither t of
  Left msg -> error msg
  Right a -> a

readEither :: Read a => Text -> Either String a
readEither = parseOnly (parsePrec 0)


-- Numeric instances (from Data.Attoparsec.Text)
instance Read Int     where parsePrec = atto decimal; parsePrefix = decimal
instance Read Int8    where parsePrec = atto decimal; parsePrefix = decimal
instance Read Int16   where parsePrec = atto decimal; parsePrefix = decimal
instance Read Int32   where parsePrec = atto decimal; parsePrefix = decimal
instance Read Int64   where parsePrec = atto decimal; parsePrefix = decimal
instance Read Integer where parsePrec = atto decimal; parsePrefix = decimal
instance Read Word    where parsePrec = atto decimal; parsePrefix = decimal
instance Read Word8   where parsePrec = atto decimal; parsePrefix = decimal
instance Read Word16  where parsePrec = atto decimal; parsePrefix = decimal
instance Read Word32  where parsePrec = atto decimal; parsePrefix = decimal
instance Read Word64  where parsePrec = atto decimal; parsePrefix = decimal

-- Float instances (from Data.Double.Conversion.Text)
instance Read Double where parsePrec = atto rational; parsePrefix = rational
instance Read Float  where parsePrec = atto rational; parsePrefix = rational

-- | 1 or more parens. Consumes all whitespace and parens before p, and whitespace after p, followed by the matching close parens.
parens1_ :: Parser a -> ParserPrec a
parens1_ p = parens_ (atto (paren p))

parens1__ :: Parser a -> Parser a
parens1__ p = parens1_ p 0

-- | Consume a comma surrounded by whitespace
comma_ :: Parser ()
comma_ = skipSpace >> char ',' >> skipSpace

-- Custom tuple instances
instance Read () where
  parsePrec = parens1_ $ pure ()
  {-# INLINABLE parsePrec #-}
  parsePrefix = parens1__ $ pure ()
  {-# INLINABLE parsePrefix #-}
instance (Read a, Read b) => Read (a, b) where
  parsePrec = parens1_ ((,) <$> parsePrec 0 <* comma_ <*> parsePrec 0)
  {-# INLINABLE parsePrec #-}
  parsePrefix = parens1__ ((,) <$> parsePrefix <* comma_ <*> parsePrefix)
  {-# INLINABLE parsePrefix #-}
{-
instance (Read a, Read b, Read c) => Read (a, b, c) where
  showPrec _ (a, b, c) = "(" <> show a <> "," <> show b <> "," <> show c <> ")"
  {-# INLINABLE showPrec #-}
  showPrefix = defaultReadPrefix
  {-# INLINABLE showPrefix #-}
instance (Read a, Read b, Read c, Read d) => Read (a, b, c, d) where
  showPrec _ (a, b, c, d) = "(" <> show a <> "," <> show b <> "," <> show c <> "," <> show d <> ")"
  {-# INLINABLE showPrec #-}
  showPrefix = defaultReadPrefix
  {-# INLINABLE showPrefix #-}
instance (Read a, Read b, Read c, Read d, Read e) => Read (a, b, c, d, e) where
  showPrec _ (a, b, c, d, e) = "(" <> show a <> "," <> show b <> "," <> show c <> "," <> show d <> "," <> show e <> ")"
  {-# INLINABLE showPrec #-}
  showPrefix = defaultReadPrefix
  {-# INLINABLE showPrefix #-}
instance (Read a, Read b, Read c, Read d, Read e, Read f) => Read (a, b, c, d, e, f) where
  showPrec _ (a, b, c, d, e, f) = "(" <> show a <> "," <> show b <> "," <> show c <> "," <> show d <> "," <> show e <> "," <> show f <> ")"
  {-# INLINABLE showPrec #-}
  showPrefix = defaultReadPrefix
  {-# INLINABLE showPrefix #-}
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g) => Read (a, b, c, d, e, f, g) where
  showPrec _ (a, b, c, d, e, f, g) = "(" <> show a <> "," <> show b <> "," <> show c <> "," <> show d <> "," <> show e <> "," <> show f <> "," <> show g <> ")"
  {-# INLINABLE showPrec #-}
  showPrefix = defaultReadPrefix
  {-# INLINABLE showPrefix #-}
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h) => Read (a, b, c, d, e, f, g, h) where
  showPrec _ (a, b, c, d, e, f, g, h) = "(" <> show a <> "," <> show b <> "," <> show c <> "," <> show d <> "," <> show e <> "," <> show f <> "," <> show g <> "," <> show h <> ")"
  {-# INLINABLE showPrec #-}
  showPrefix = defaultReadPrefix
  {-# INLINABLE showPrefix #-}
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h, Read i) => Read (a, b, c, d, e, f, g, h, i) where
  showPrec _ (a, b, c, d, e, f, g, h, i) = "(" <> show a <> "," <> show b <> "," <> show c <> "," <> show d <> "," <> show e <> "," <> show f <> "," <> show g <> "," <> show h <> "," <> show i <> ")"
  {-# INLINABLE showPrec #-}
  showPrefix = defaultReadPrefix
  {-# INLINABLE showPrefix #-}
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h, Read i, Read j) => Read (a, b, c, d, e, f, g, h, i, j) where
  showPrec _ (a, b, c, d, e, f, g, h, i, j) = "(" <> show a <> "," <> show b <> "," <> show c <> "," <> show d <> "," <> show e <> "," <> show f <> "," <> show g <> "," <> show h <> "," <> show i <> "," <> show j <> ")"
  {-# INLINABLE showPrec #-}
  showPrefix = defaultReadPrefix
  {-# INLINABLE showPrefix #-}

-}
