{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleInstances, ScopedTypeVariables #-}
module Data.Text.Serialize.Read.Generic where

import Control.Applicative

import Data.String
import GHC.Generics hiding (prec)
import Data.Text.Serialize.Read.Class
import qualified Data.Text as T
import Prelude hiding (Read(..))

appPrec = 10

instance GRead f => GRead (M1 D m f) where
  {-# INLINE greadPrec #-}
  greadPrec = M1 <$> greadPrec

instance (GRead f, GRead g) => GRead (f :+: g) where
  {-# INLINE greadPrec #-}
  greadPrec = (L1 <$> greadPrec) <|> (R1 <$> greadPrec)

instance (Constructor c, GReadFields f) => GRead (M1 C c f) where
  {-# INLINE greadPrec #-}
  greadPrec =
    let 
      con = undefined :: x c f y
      name = T.pack (conName con)
      isRecord = conIsRecord con
      fixity = conFixity con
      isEmpty = gisEmpty (undefined :: x f y)
    in
    if isRecord 
    then
      parens $ prec appPrec $
        M1 <$> (ident name *> punc "{" *> greadCommas <* punc "}")
    else
      if isEmpty
      then 
        parens $ (ident name *> (M1 <$> greadSpaces))
      else
        case fixity of
          Prefix ->
            parens $ prec appPrec $
              M1 <$> (ident name *> greadSpaces)
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
  greadSpaces = return U1
  {-# INLINE gisEmpty #-}
  gisEmpty _ = True

instance (Read field, Selector sel) => GReadFields (M1 S sel (K1 i field)) where
  {-# INLINE greadCommas #-}
  greadCommas = 
    let
      name = fromString $ selName (undefined :: x sel (K1 i field) y)
    in
     (M1 . K1) <$> (ident name *> punc "=" *> reset readPrec)
  {-# INLINE greadSpaces #-}
  greadSpaces = (M1 . K1) <$> (step readPrec)
  {-# INLINE gisEmpty #-}
  gisEmpty _ = False

instance (GReadFields f, GReadFields g) => GReadFields (f :*: g) where
  {-# INLINE greadCommas #-}
  greadCommas = (:*:) <$> greadCommas <* punc "," <*> greadCommas
  {-# INLINE greadSpaces #-}
  greadSpaces = (:*:) <$> greadSpaces <*> greadSpaces
  {-# INLINE gisEmpty #-}
  gisEmpty _ = False

panic msg = error ("Data.Text.Read.Generic: " ++ msg)
