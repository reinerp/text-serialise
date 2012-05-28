{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleInstances, ScopedTypeVariables #-}
module Data.Text.Serialize.Read.Generic where

import Control.Applicative

import Data.String
import GHC.Generics hiding (prec)
import Data.Text.Serialize.Read.Class
import Data.Text.Serialize.Read.Lex
import qualified Data.Text as T
import Prelude hiding (Read(..))

appPrec = 10

instance GRead f => GRead (M1 D m f) where
  {-# INLINE greadPrec #-}
  greadPrec = parens_ $ \n -> M1 <$> greadPrec n

instance (GRead f, GRead g) => GRead (f :+: g) where
  {-# INLINE greadPrec #-}
  greadPrec n = (L1 <$> greadPrec n) <|> (R1 <$> greadPrec n)

instance (Constructor c, GReadFields f) => GRead (M1 C c f) where
  {-# INLINE greadPrec #-}
  greadPrec =
    let 
      con = undefined :: x c f y
      {-# NOINLINE name #-}
      name = T.pack (conName con)
      isRecord = conIsRecord con
      fixity = conFixity con
      isEmpty = gisEmpty (undefined :: x f y)
    in
    if isRecord 
    then
      prec appPrec $
        \n -> M1 <$> (ident' name *> punc '{' *> greadCommas n <* punc '}')
    else
      if isEmpty
      then 
        \n -> ident' name *> (M1 <$> greadSpaces n)
      else
        case fixity of
          Prefix ->
            prec appPrec $
              \n -> M1 <$> (ident' name *> greadSpaces n)
          Infix assoc opPrec -> panic "Infix not yet supported!"                       

-- fields of a constructor
class GReadFields f where
  -- read the fields, separated by commas, and with record syntax
  greadCommas :: ParserPrec (f x)
  -- read the fields, separated by spaces
  greadSpaces :: ParserPrec (f x)
  -- is the set of fields empty?
  gisEmpty :: t f x -> Bool

instance GReadFields U1 where
  greadCommas = panic "read commas on empty!"
  {-# INLINE greadSpaces #-}
  greadSpaces = \_ -> return U1
  {-# INLINE gisEmpty #-}
  gisEmpty _ = True

instance (Read field, Selector sel) => GReadFields (M1 S sel (K1 i field)) where
  {-# INLINE greadCommas #-}
  greadCommas = 
    let
      {-# NOINLINE name #-}
      name = fromString $ selName (undefined :: x sel (K1 i field) y)
    in
     \_ -> (M1 . K1) <$> (ident name *> symbolPunc '=' *> readPrec 0)
  {-# INLINE greadSpaces #-}
  greadSpaces = \n -> (M1 . K1) <$> readPrec (n+1)
  {-# INLINE gisEmpty #-}
  gisEmpty _ = False

instance (GReadFields f, GReadFields g) => GReadFields (f :*: g) where
  {-# INLINE greadCommas #-}
  greadCommas = \n -> (:*:) <$> greadCommas n <* punc ',' <*> greadCommas n
  {-# INLINE greadSpaces #-}
  greadSpaces = \n -> (:*:) <$> greadSpaces n <*> greadSpaces n
  {-# INLINE gisEmpty #-}
  gisEmpty _ = False

panic msg = error ("Data.Text.Read.Generic: " ++ msg)
