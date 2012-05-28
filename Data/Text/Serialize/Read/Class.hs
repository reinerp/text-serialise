{-# LANGUAGE DefaultSignatures, FlexibleContexts, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Data.Text.Serialize.Read.Class where

import Control.Monad.Reader
import Control.Monad
import Data.Attoparsec.Text
import GHC.Generics
import Control.Applicative
import Prelude hiding (Read(..))
import qualified Data.Text.Serialize.Read.Lex as Lex
import qualified Data.Text as T

class Read a where
  readPrec :: ParserPrec a

  default readPrec :: (Generic a, GRead (Rep a)) => ParserPrec a
  readPrec = to <$> greadPrec

class GRead f where
  greadPrec :: ParserPrec (f x)

----------------------------------------------------------------------------------------------------
-- ParserPrec and friends

-- | An attoparsec 'Parser' together with parenthesis information.
newtype ParserPrec a = ParserPrec { unParserPrec :: ReaderT Int Parser a }
  deriving (Monad, Functor, MonadPlus, Applicative, Alternative)

runParserPrec :: ParserPrec a -> Int -> Parser a
runParserPrec = runReaderT . unParserPrec

atto :: Parser a -> ParserPrec a
atto p = ParserPrec (lift p)

{-# INLINE punc #-}
punc :: T.Text -> ParserPrec ()
punc = atto . Lex.lexed . Lex.punc

ident :: T.Text -> ParserPrec ()
ident name = do
  name' <- atto (Lex.lexed Lex.ident)
  guard (name == name')

parens :: ParserPrec a -> ParserPrec a
parens p = optional
  where
    optional = p <|> mandatory
    mandatory = paren optional

paren :: ParserPrec a -> ParserPrec a
paren p = punc "(" *> reset p <* punc ")"

prec :: Int -> ParserPrec a -> ParserPrec a
prec n (ParserPrec (ReaderT p)) = ParserPrec . ReaderT $ \n' -> if n' <= n then p n else empty

step :: ParserPrec a -> ParserPrec a
step (ParserPrec p) = ParserPrec (local (+1) p)

reset :: ParserPrec a -> ParserPrec a
reset (ParserPrec p) = ParserPrec (local (const 0) p)
