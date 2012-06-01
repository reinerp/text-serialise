{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-} 
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Text.Serialize.Show.Generic() where

import Data.Text.Serialize.Show.Class
import GHC.Generics
import Data.Monoid
import qualified Data.Text as SText
import qualified Data.Text.Lazy.IO as Text
import Data.Text.Lazy.Builder

import qualified Prelude
import Prelude hiding (Show(..))

appPrec = 10
__ = undefined

instance GShow f => GShow (M1 D m f) where
  {-# INLINE gshowPrec #-}
  gshowPrec prec (M1 f) = gshowPrec prec f
  {-# INLINE gshowPrefix #-}
  gshowPrefix (M1 f) = gshowPrefix f

instance (GShow f, GShow g) => GShow (f :+: g) where
  {-# INLINE gshowPrec #-}
  gshowPrec prec (L1 f) = gshowPrec prec f
  gshowPrec prec (R1 g) = gshowPrec prec g
  {-# INLINE gshowPrefix #-}
  gshowPrefix (L1 f) = gshowPrefix f
  gshowPrefix (R1 g) = gshowPrefix g

instance (Constructor c, GShowFields f) => GShow (M1 C c f) where
  {-# INLINE gshowPrec #-}
  gshowPrec
    | isRecord = 
        \prec (M1 fields) ->
        if prec > appPrec
        then fromText ("(" <> name <> " {") <> gshowCommas fields <> "})"
        else fromText (name <> " {") <> gshowCommas fields <> singleton '}'
    | numFields == 0 =
        \_ _ -> fromText name
    | Infix assoc opPrec <- fixity, numFields == 2 =
        error "Infix not yet supported!"
    | otherwise =
        \prec (M1 fields) -> 
          if prec > appPrec
          then fromText ("(" <> name <> " ") <> gshowSpaces fields <> singleton ')'
          else fromText (name <> " ") <> gshowSpaces fields
   where
    con = __ :: x c f y
    name = SText.pack (conName con)
    isRecord = conIsRecord con
    fixity = conFixity con
    numFields = gnumFields (__ :: f y)

  {-# INLINE gshowPrefix #-}
  gshowPrefix = \(M1 fields) -> fromText ("(" <> name <> " ") <> gshowSpaces fields <> singleton ')'
   where
    name = SText.pack (conName (__ :: x c f y))

-- fields of a constructor
class GShowFields f where
  -- show the fields, separated by commas, and with record syntax
  gshowCommas :: f x -> Builder
  -- show the fields, separated by spaces
  gshowSpaces :: f x -> Builder
  -- is the set of fields empty?
  gnumFields :: f x -> Int

instance GShowFields U1 where
  gshowCommas = error "show commas on empty!"
  gshowSpaces = error "show spaces on empty!"
  {-# INLINE gnumFields #-}
  gnumFields _ = 0

instance (Show field, Selector sel) => GShowFields (M1 S sel (K1 i field)) where
  {-# INLINE gshowCommas #-}
  gshowCommas = 
    let
      name = fromString $ selName (__ :: x sel (K1 i field) y)
    in
     \(M1 (K1 field)) -> name <> " = " <> showPrec 0 field
  {-# INLINE gshowSpaces #-}
  gshowSpaces (M1 (K1 field)) = showPrec (appPrec+1) field
  {-# INLINE gnumFields #-}
  gnumFields _ = 1

instance (GShowFields f, GShowFields g) => GShowFields (f :*: g) where
  {-# INLINE gshowCommas #-}
  gshowCommas (f :*: g) = gshowCommas f <> ", " <> gshowCommas g
  {-# INLINE gshowSpaces #-}
  gshowSpaces (f :*: g) = gshowSpaces f <> singleton ' ' <> gshowSpaces g
  {-# INLINE gnumFields #-}
  gnumFields _ = gnumFields (__ :: f x) + gnumFields (__ :: g x)

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