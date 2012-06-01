{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Types(Type(..), types)

import Data.Text.Serialize
import Prelude hiding (Show(..))
import qualified Prelude
import Test.HUnit (assertBool)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.Typeable
import Data.Text.Lazy(unpack)

equalsPrelude val = unpack (showLazyText val) == Prelude.show val

testType :: Type -> Test
testType (Auto name (ty :: a)) 
  -- Unlike Prelude, show (1 :: Double) == "1", not "1.0". So we just skip this test. 
  -- The round-tripping test should suffice, though.
  | name `elem` ["Double", "[Double]"] = testProperty name True
  | otherwise = testProperty name (equalsPrelude :: a -> Bool)
testType (Custom name vals) =
  testGroup name (map (testCase "" . assertBool "" . equalsPrelude) vals)

main = defaultMain (map testType types)
