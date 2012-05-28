{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, OverloadedStrings #-}
module Main(main) where

import qualified Criterion.Main as C
import GHC.Generics (Generic)
import qualified Data.Text as S
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy as T
import qualified Data.Text.Serialize.Show as T
import Data.Typeable

import Debug.Trace
import Test.QuickCheck.Arbitrary(Arbitrary(..))
import Test.QuickCheck.Gen(Gen(..))
import System.Random

import Data.Monoid

severalBufSize = 10^3
size = 10^3
__ = undefined
seed = 0

{-# INLINABLE customBench #-}
customBench :: (Typeable a, T.Show a, Show a) => a -> C.Benchmark
customBench value = C.bgroup (show (typeOf value)) [
  C.bgroup "single" [
     C.bench "String" $ C.nf (T.pack . show) value,
     C.bench "Builder" $ C.nf (B.toLazyText . T.show) value
     ],
  C.bgroup "many" [
    C.bench "String" $ C.nf (T.pack . showMany n) value,
    C.bench "Builder" $ C.nf (B.toLazyText . showManyT n) value
    ]
  ]
  where n = severalBufSize `div` length (show value) + 1 :: Int

showMany 0 val = ""
showMany n val = shows val (showMany (n-1) val)
{-# INLINABLE showMany #-}

showManyT 0 val = mempty
showManyT n val = T.show val <> showManyT (n-1) val
{-# INLINABLE showManyT #-}

{-# INLINABLE bench #-}
bench :: forall a. (Arbitrary a, Typeable a, T.Show a, Show a) => a -> C.Benchmark
bench _ = customBench (unGen arbitrary (mkStdGen seed) size :: a)

main = C.defaultMain [
  bench (__ :: Double),
  bench (__ :: Int),
  customBench ("123456" :: String),
  bench (__ :: Bool),
  bench (__ :: [Bool]),
  bench (__ :: [Double]),
  bench (__ :: [Int]),
  bench (__ :: Char),
  bench (["123456","789012"] :: [String]),
  customBench ("123456" :: T.Text),
  customBench ("123456" :: S.Text),
  bench (__ :: Either Int Int),
  bench (__ :: ()),
  bench (__ :: [()]),
  bench (__ :: ((),())),
  bench (__ :: (Int, Int)),
  bench (__ :: [(Int, Int)])
  ]
