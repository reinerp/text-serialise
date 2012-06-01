{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleInstances, ScopedTypeVariables, PatternGuards #-}
module Data.Text.Serialize.Read.Generic where

import Control.Applicative

import Data.String
import GHC.Generics hiding (prec)
import Data.Attoparsec.Text(Parser)
import Data.Text.Serialize.Common
import Data.Text.Serialize.Read.Class
import Data.Text.Serialize.Read.Lex
import qualified Data.Text as T
import Prelude hiding (Read(..))

instance GRead f => GRead (M1 D m f) where
  {-# INLINE gparsePrec #-}
  gparsePrec = parens_ $ \n -> M1 <$> gparsePrec n
  {-# INLINE gparsePrefix #-}
  gparsePrefix = paren gparsePrefix

instance (GRead f, GRead g) => GRead (f :+: g) where
  {-# INLINE gparsePrec #-}
  gparsePrec n = (L1 <$> gparsePrec n) <|> (R1 <$> gparsePrec n)
  {-# INLINE gparsePrefix #-}
  gparsePrefix = (L1 <$> gparsePrefix) <|> (R1 <$> gparsePrefix)

instance (Constructor c, GReadFields f) => GRead (M1 C c f) where
  {-# INLINE gparsePrec #-}
  gparsePrec
    | isRecord =
        prec appPrec $
        \n -> M1 <$> (ident' name *> punc '{' *> greadCommas n <* punc '}')
    | numFields == 0 =
        \n -> ident' name *> (M1 <$> greadSpaces n)
    | Infix assoc opPrec <- fixity, numFields == 2 =
        panic "Infix not yet supported!"
    | otherwise =
        prec appPrec $
        \n -> M1 <$> (ident' name *> greadSpaces n)
   where
    con = __ :: x c f y
    {-# NOINLINE name #-}
    name = T.pack (conName con)
    isRecord = conIsRecord con
    fixity = conFixity con
    numFields = gnumFields (__ :: f y)

  {-# INLINE gparsePrefix #-}
  gparsePrefix = M1 <$> (ident' name *> greadSpacesPrefix)
   where
    {-# NOINLINE name #-}
    name = T.pack (conName (__ :: x c f y))

-- fields of a constructor
class GReadFields f where
  -- read the fields, separated by commas, and with record syntax
  greadCommas :: ParserPrec (f x)
  -- read the fields, separated by spaces
  greadSpaces :: ParserPrec (f x)
  -- read the fields, separated by spaces, in prefix mode
  greadSpacesPrefix :: Parser (f x)
  -- is the set of fields empty?
  gnumFields :: f x -> Int

instance GReadFields U1 where
  greadCommas = panic "read commas on empty!"
  {-# INLINE greadSpaces #-}
  greadSpaces = \_ -> return U1
  {-# INLINE greadSpacesPrefix #-}
  greadSpacesPrefix = return U1
  {-# INLINE gnumFields #-}
  gnumFields _ = 0

instance (Read field, Selector sel) => GReadFields (M1 S sel (K1 i field)) where
  {-# INLINE greadCommas #-}
  greadCommas = 
    let
      {-# NOINLINE name #-}
      name = fromString $ selName (__ :: x sel (K1 i field) y)
    in
     \_ -> (M1 . K1) <$> (ident name *> symbolPunc '=' *> parsePrec 0)
  {-# INLINE greadSpaces #-}
  greadSpaces = \n -> (M1 . K1) <$> parsePrec (n+1)
  {-# INLINE greadSpacesPrefix #-}
  greadSpacesPrefix = (M1 . K1) <$> parsePrefix
  {-# INLINE gnumFields #-}
  gnumFields _ = 1

instance (GReadFields f, GReadFields g) => GReadFields (f :*: g) where
  {-# INLINE greadCommas #-}
  greadCommas = \n -> (:*:) <$> greadCommas n <* punc ',' <*> greadCommas n
  {-# INLINE greadSpaces #-}
  greadSpaces = \n -> (:*:) <$> greadSpaces n <*> greadSpaces n
  {-# INLINE greadSpacesPrefix #-}
  greadSpacesPrefix = (:*:) <$> greadSpacesPrefix <*> greadSpacesPrefix
  {-# INLINE gnumFields #-}
  gnumFields _ = gnumFields (__ :: f x) + gnumFields (__ :: g x)

panic msg = error ("Data.Text.Serialize.Read.Generic: " ++ msg)
