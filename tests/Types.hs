{-# LANGUAGE ExistentialQuantification, OverloadedStrings, ScopedTypeVariables #-}
module Types(Type(..), typeName, types) where

import Prelude hiding (Read(..), Show(..))
import qualified Prelude
import Data.Text.Serialize
import Data.Typeable
import qualified Data.Text as S
import qualified Data.Text.Lazy as T
import Test.QuickCheck.Arbitrary(Arbitrary)

data Type
 = forall a.
    (
--     Read a,
     Show a,
     Prelude.Read a,
     Prelude.Show a,
     Eq a,
     Arbitrary a
    )
    => Auto String a
 | forall a.
    (
--     Read a,
     Show a,
     Prelude.Read a,
     Prelude.Show a,
     Eq a
    )
    => Custom String [a]

typeName :: Type -> String
typeName (Auto nm _) = nm
typeName (Custom nm _) = nm

__ = undefined

auto ty = Auto (Prelude.show (typeOf ty)) ty
custom (vals :: [ty]) = Custom (Prelude.show (typeOf (__ :: ty))) vals

types :: [Type]
types = [
  auto (__ :: Double),
  auto (__ :: Int),
  custom ["123456" :: String],
  auto (__ :: Bool),
  auto (__ :: [Bool]),
  auto (__ :: [Double]),
  auto (__ :: [Int]),
  auto (__ :: Char),
  custom [["123456","789012"] :: [String]],
  custom ["123456" :: T.Text],
  custom ["123456" :: S.Text],
  auto (__ :: Either Int Int),
  auto (__ :: ()),
  auto (__ :: [()]),
  auto (__ :: ((),())),
  auto (__ :: (Int, Int)),
  auto (__ :: [(Int, Int)])
 ]
