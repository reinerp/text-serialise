{-# LANGUAGE TypeOperators, ScopedTypeVariables, FlexibleInstances, 
             OverloadedStrings, DefaultSignatures, FlexibleContexts, 
             BangPatterns #-}

module Data.Text.Serialize.Show.Generic( ) where

import Data.Text.Serialize.Show.Class
import GHC.Generics
import Data.Monoid
import qualified Data.Text as SText
import qualified Data.Text.Lazy.IO as Text
import Data.Text.Lazy.Builder

import qualified Prelude
import Prelude hiding (Show(..))

appPrec = 10

instance GShow f => GShow (M1 D m f) where
  {-# INLINE gshowPrec #-}
  gshowPrec prec (M1 f) = gshowPrec prec f

instance (GShow f, GShow g) => GShow (f :+: g) where
  {-# INLINE gshowPrec #-}
  gshowPrec prec (L1 f) = gshowPrec prec f
  gshowPrec prec (R1 g) = gshowPrec prec g

instance (Constructor c, GShowFields f) => GShow (M1 C c f) where
  {-# INLINE gshowPrec #-}
  gshowPrec =
    let 
      con = undefined :: x c f y
      name = SText.pack (conName con)
      isRecord = conIsRecord con
      fixity = conFixity con
      isEmpty = gisEmpty (undefined :: x f y)
    in
    if isRecord 
    then
      \prec (M1 fields) ->
        if prec > appPrec
        then fromText ("(" <> name <> " {") <> gshowCommas fields <> "})"
        else fromText (name <> " {") <> gshowCommas fields <> singleton '}'
    else
      if isEmpty
      then 
        \_ _ -> fromText name
      else
        case fixity of
          Prefix ->
            \prec (M1 fields) -> 
            if prec > appPrec
            then fromText ("(" <> name <> " ") <> gshowSpaces fields <> singleton ')'
            else fromText (name <> " ") <> gshowSpaces fields
          Infix assoc opPrec -> error "Infix not yet supported!"                       

-- fields of a constructor
class GShowFields f where
  -- show the fields, separated by commas, and with record syntax
  gshowCommas :: f x -> Builder
  -- show the fields, separated by spaces
  gshowSpaces :: f x -> Builder
  -- is the set of fields empty?
  gisEmpty :: t f x -> Bool

instance GShowFields U1 where
  gshowCommas = error "show commas on empty!"
  gshowSpaces = error "show spaces on empty!"
  {-# INLINE gisEmpty #-}
  gisEmpty _ = True

instance (Show field, Selector sel) => GShowFields (M1 S sel (K1 i field)) where
  {-# INLINE gshowCommas #-}
  gshowCommas = 
    let
      name = fromString $ selName (undefined :: x sel (K1 i field) y)
    in
     \(M1 (K1 field)) -> name <> " = " <> showPrec 0 field
  {-# INLINE gshowSpaces #-}
  gshowSpaces (M1 (K1 field)) = showPrec (appPrec+1) field
  {-# INLINE gisEmpty #-}
  gisEmpty _ = False

instance (GShowFields f, GShowFields g) => GShowFields (f :*: g) where
  {-# INLINE gshowCommas #-}
  gshowCommas (f :*: g) = gshowCommas f <> ", " <> gshowCommas g
  {-# INLINE gshowSpaces #-}
  gshowSpaces (f :*: g) = gshowSpaces f <> singleton ' ' <> gshowSpaces g
  {-# INLINE gisEmpty #-}
  gisEmpty _ = False

panic msg = error ("Data.Text.Show.Generic: " ++ msg)

-- showing empty constructors
{-instance Constructor c => GShow (C1 c U1) where
  gshowPrec _ (M1 U1) = fromString (conName (undefined :: x c U1 z))
-}
--showing 
--class GShowFields f where
--  gshowCon :: 

{-
data Empty
--deriving instance Prelude.Show Empty

data A = A Int Int | B | C { foo :: Bool, bar :: Bool } | D {} | (:+) { plus :: Int } | A :* A
  deriving(Generic, Prelude.Show)

data Z a = Z a
  deriving(Generic)-}