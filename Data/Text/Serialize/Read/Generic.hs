module Data.Text.Serialize.Read.Generic where

import Control.Applicative

import Data.Text.Serialize.Read.Class

appPrec = 10

instance GRead f => GRead (M1 D m f) where
  {-# INLINE greadPrec #-}
  greadPrec prec = M1 <$> greadPrec prec

instance (GRead f, GRead g) => GRead (f :+: g) where
  {-# INLINE greadPrec #-}
  greadPrec prec = (L1 <$> greadPrec prec) <|> (R1 <$> greadPrec prec)

instance (Constructor c, GReadFields f) => GRead (M1 C c f) where
  {-# INLINE greadPrec #-}
  greadPrec =
    let 
      con = undefined :: x c f y
      name = SText.pack (conName con)
      isRecord = conIsRecord con
      fixity = conFixity con
      isEmpty = gisEmpty (undefined :: x f y)
    in
    if isRecord 
    then
      \n (M1 fields) ->
        parens $ prec if prec > appPrec
        then fromText ("(" <> name <> " {") <> greadCommas fields <> "})"
        else fromText (name <> " {") <> greadCommas fields <> singleton '}'
    else
      if isEmpty
      then 
        \_ _ -> fromText name
      else
        case fixity of
          Prefix ->
            \prec (M1 fields) -> 
            if prec > appPrec
            then fromText ("(" <> name <> " ") <> greadSpaces fields <> singleton ')'
            else fromText (name <> " ") <> greadSpaces fields
          Infix assoc opPrec -> error "Infix not yet supported!"                       

-- fields of a constructor
class GReadFields f where
  -- read the fields, separated by commas, and with record syntax
  greadCommas :: f x -> Builder
  -- read the fields, separated by spaces
  greadSpaces :: f x -> Builder
  -- is the set of fields empty?
  gisEmpty :: t f x -> Bool

instance GReadFields U1 where
  greadCommas = error "read commas on empty!"
  greadSpaces = error "read spaces on empty!"
  {-# INLINE gisEmpty #-}
  gisEmpty _ = True

instance (Read field, Selector sel) => GReadFields (M1 S sel (K1 i field)) where
  {-# INLINE greadCommas #-}
  greadCommas = 
    let
      name = fromString $ selName (undefined :: x sel (K1 i field) y)
    in
     \(M1 (K1 field)) -> name <> " = " <> readPrec 0 field
  {-# INLINE greadSpaces #-}
  greadSpaces (M1 (K1 field)) = readPrec (appPrec+1) field
  {-# INLINE gisEmpty #-}
  gisEmpty _ = False

instance (GReadFields f, GReadFields g) => GReadFields (f :*: g) where
  {-# INLINE greadCommas #-}
  greadCommas (f :*: g) = greadCommas f <> ", " <> greadCommas g
  {-# INLINE greadSpaces #-}
  greadSpaces (f :*: g) = greadSpaces f <> singleton ' ' <> greadSpaces g
  {-# INLINE gisEmpty #-}
  gisEmpty _ = False

panic msg = error ("Data.Text.Read.Generic: " ++ msg)

-- reading empty constructors
{-instance Constructor c => GRead (C1 c U1) where
  greadPrec _ (M1 U1) = fromString (conName (undefined :: x c U1 z))
-}
--reading 
--class GReadFields f where
--  greadCon :: 

{-
data Empty
--deriving instance Prelude.Read Empty

data A = A Int Int | B | C { foo :: Bool, bar :: Bool } | D {} | (:+) { plus :: Int } | A :* A
  deriving(Generic, Prelude.Read)

data Z a = Z a
  deriving(Generic)-}