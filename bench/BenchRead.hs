{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main(main) where

import Control.Monad
import Control.Applicative
import Data.Attoparsec.Text hiding (Number)
import Criterion.Main
import GHC.Generics
import Data.Text.Serialize.Read
import qualified Prelude
import Prelude hiding (Read(..), read)
import Data.Text(Text)
import qualified Data.Text as T

data Number = Zero | Suc Number
  deriving (Generic, Prelude.Show, Prelude.Read)

instance Read Number

main = defaultMain [
  bgroup "Suc Zero" [
     bench "text" $ whnf (read :: Text -> Number) "Suc Zero",
     bench "string" $ whnf (Prelude.read :: String -> Number) "Suc Zero",
     bench "text-via-string" $ whnf (Prelude.read . T.unpack :: Text -> Number) "Suc Zero",
     bench "raw" $ whnf plainRead "Suc Zero",
     bench "slow-raw" $ whnf slowPlainRead "Suc Zero"
     ],
  bgroup "Zero" [
     bench "text" $ whnf (read :: Text -> Number) "Zero",
     bench "string" $ whnf (Prelude.read :: String -> Number) "Zero",
     bench "text-via-string" $ whnf (Prelude.read . T.unpack :: Text -> Number) "Zero",
     bench "raw" $ whnf plainRead "Zero",
     bench "slow-raw" $ whnf slowPlainRead "Zero"
     ]
  ]

plainRead :: Text -> Number
plainRead t = case parseOnly p t of
  Left msg -> error msg
  Right n -> n
 where
   p = skipSpace >> ((Zero <$ string "Zero") <|> (Suc <$> (string "Suc" *> p)))

slowString s = do
  str <- string s
  guard (s == str)

slowPlainRead :: Text -> Number
slowPlainRead t = case parseOnly p t of
  Left msg -> error msg
  Right n -> n
 where
   p = (Zero <$ slowString "Zero") <|> (Suc <$> (slowString "Suc " *> p))
