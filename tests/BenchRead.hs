{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main(main) where

import Data.String
import Control.Monad
import Control.Applicative
import Data.Attoparsec.Text hiding (Number)
import Criterion.Main
import GHC.Generics
import Data.Text.Serialize
import qualified Prelude
import Prelude hiding (Read(..), read)
import Data.Text(Text)
import qualified Data.Text as T

data Number = Zero | Suc !Number
  deriving (Generic, Prelude.Show, Prelude.Read)

instance Read Number

main = defaultMain [
  let 
    contents :: IsString a => a
    contents = "Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc Zero)))))))))))))))))))"
    contents2 = T.filter (\c -> c /= '(' && c /= ')') contents
  in bgroup "Suc^20 Zero" [
     bench "text" $ whnf (read :: Text -> Number) contents,
     bench "string" $ whnf (Prelude.read :: String -> Number) contents,
     bench "text-via-string" $ whnf (Prelude.read . T.unpack :: Text -> Number) contents,
     bench "raw" $ whnf plainRead contents2
     ],
  bgroup "Suc (Suc Zero)" [
     bench "text" $ whnf (read :: Text -> Number) "Suc (Suc Zero)",
     bench "string" $ whnf (Prelude.read :: String -> Number) "Suc (Suc Zero)",
     bench "text-via-string" $ whnf (Prelude.read . T.unpack :: Text -> Number) "Suc (Suc Zero)",
     bench "raw" $ whnf plainRead "Suc Suc Zero"
     ],
  bgroup "Suc Zero" [
     bench "text" $ whnf (read :: Text -> Number) "Suc Zero",
     bench "string" $ whnf (Prelude.read :: String -> Number) "Suc Zero",
     bench "text-via-string" $ whnf (Prelude.read . T.unpack :: Text -> Number) "Suc Zero",
     bench "raw" $ whnf plainRead "Suc Zero"
     ],
  bgroup "Zero" [
     bench "text" $ whnf (read :: Text -> Number) "Zero",
     bench "string" $ whnf (Prelude.read :: String -> Number) "Zero",
     bench "text-via-string" $ whnf (Prelude.read . T.unpack :: Text -> Number) "Zero",
     bench "raw" $ whnf plainRead "Zero"
     ]
  ]

plainRead :: Text -> Number
plainRead t = case parseOnly p t of
  Left msg -> error msg
  Right n -> n
 where
   p = skipSpace >> ((Zero <$ string "Zero") <|> (Suc <$> (string "Suc" *> p)))
