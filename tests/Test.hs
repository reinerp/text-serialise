{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module Main (main) where

import Types(Type(..), typeName, types)

import Data.Text.Serialize
import Prelude hiding (Show(..), Read(..))
import qualified Prelude
import Test.HUnit (assertBool)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.Typeable
import Data.Text.Lazy(unpack)

infixl 2 <:
(<:) :: String -> (Type -> Test) -> (String, Type -> Test)
a <: b = (a, b)

tests :: [(String, Type -> Test)]
tests = [
  -- show agrees with prelude
  "equalsPrelude" <: property (\val -> unpack (showLazyText val) == Prelude.show val)
    `except` ["Double", "[Double]"]
 ]

property :: (forall a. ({-Read a,-} Show a, {-Prelude.Read a,-} Prelude.Show a, Eq a) => a -> Bool)
         -> (Type -> Test)
property prop (Auto name (ty :: a)) = testProperty name (prop :: a -> Bool)
property prop (Custom name vals)
 = testGroup name [testCase (Prelude.show val) $ assertBool "" (prop val) | val <- vals]

except :: (Type -> Test) -> [String] -> (Type -> Test)
test `except` types = \ty -> if typeName ty `elem` types
                             then testProperty (typeName ty ++ " (SKIPPED)") True
                             else test ty

main = defaultMain [testGroup name (map test types) | (name, test) <- tests]
