module Data.Text.Serialize.Read(
  Read(..), 
  read,
  readEither,
  ParserPrec(..),
 ) where

import Prelude hiding (Read(..), read)
import Data.Text.Serialize.Read.Class
import Data.Text.Serialize.Read.Generic()
import Data.Attoparsec.Text(parseOnly)
import Data.Text(Text)

read :: Read a => Text -> a
read t = case readEither t of
  Left msg -> error msg
  Right a -> a

readEither :: Read a => Text -> Either String a
readEither = parseOnly (runParserPrec readPrec 0)
