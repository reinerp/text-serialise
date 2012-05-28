-- Adapted from Text.Read.Lex
{-# LANGUAGE OverloadedStrings, DeriveGeneric, BangPatterns #-}
module Data.Text.Serialize.Read.Lex(
  lexed,
  punc,
  operator,
  ident,
  litChar,
  litString,
 ) where

import GHC.Generics

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text as A
import Data.Bits
import Data.Char(ord, isDigit, isAlpha, isAlphaNum, chr, isSpace)
import Data.Monoid
import Data.Ratio
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Prelude hiding(lex)

    
-- | @lexed p = skipSpace >> p@
lexed :: Parser a -> Parser a
lexed p = skipSpace >> p

{-# RULES 
"punc ," punc (T.pack ",") = void $ char ','
"punc ;" punc (T.pack ";") = void $ char ';'
"punc (" punc (T.pack "(") = void $ char '('
"punc )" punc (T.pack ")") = void $ char ')'
"punc [" punc (T.pack "[") = void $ char '['
"punc ]" punc (T.pack "]") = void $ char ']'
"punc {" punc (T.pack "{") = void $ char '{'
"punc }" punc (T.pack "}") = void $ char '}'
"punc `" punc (T.pack "`") = void $ char '`'
 #-}

-- | Parse the given punctuation symbol
punc :: T.Text -> Parser ()
punc str
  | Just (c, r) <- T.uncons str
  , T.null r
  , not (isSymbolChar c)
    = void $ char c
  | otherwise = do
    str' <- operator
    guard (str == str')

-- | Operators other than the reserved operators
operator :: Parser T.Text
operator = do 
  s <- operator'
  if s `elem` reserved_ops
  then empty
  else return s
 where
  reserved_ops   = ["..", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]

-- | Operator or punctuation
operator' :: Parser T.Text
operator' = takeWhile1 isSymbolChar

isSymbolChar = inClass "!@#$%&*+./<=>?\\^|:~-"

-- | Identifiers
ident :: Parser T.Text
ident = checkedIdent =<< scan True step
  where
    checkedIdent t 
      | T.null t  = empty
      | otherwise = return t

    -- bool arg: are we looking at the first character?
    step True  c = if isIdsChar c then Just False else Nothing
    step False c = if isIdfChar c then Just False else Nothing

    -- Identifiers can start with a '_'
    isIdsChar c = isAlpha c || c == '_'
    isIdfChar c = isAlphaNum c || c `elem` "_'"

-- | Character literal
litChar :: Parser Char
litChar =
  do _ <- char '\''
     (c,esc) <- lexCharE
     guard (esc || c /= '\'')   -- Eliminate '' possibility
     _ <- char '\''
     return c

lexCharE :: Parser (Char, Bool)  -- "escaped or not"?
lexCharE =
  do c1 <- anyChar
     if c1 == '\\'
       then do c2 <- lexEsc; return (c2, True)
       else do return (c1, False)
 where
  lexEsc =
    lexEscChar
      <|> lexNumeric
        <|> lexCntrlChar
          <|> lexAscii

  lexEscChar =
    do c <- anyChar
       case c of
         'a'  -> return '\a'
         'b'  -> return '\b'
         'f'  -> return '\f'
         'n'  -> return '\n'
         'r'  -> return '\r'
         't'  -> return '\t'
         'v'  -> return '\v'
         '\\' -> return '\\'
         '\"' -> return '\"'
         '\'' -> return '\''
         _    -> empty

  lexNumeric =
    do n <-     (satisfy (\c -> c == 'x' || c == 'X') *> hexadecimal)
            <|> (satisfy (\c -> c == 'o' || c == 'O') *> octal)
            <|> decimal
       guard (n <= ord maxBound)
       return (chr n)
  
  octal :: Parser Int
  octal = T.foldl' step 0 <$> takeWhile1 (\c -> c >= '0' && c <= '7')
    where step a c = a * 10 + fromIntegral (ord c - ord '0')

  lexCntrlChar =
    do _ <- char '^'
       c <- anyChar
       case c of
         '@'  -> return '\^@'
         'A'  -> return '\^A'
         'B'  -> return '\^B'
         'C'  -> return '\^C'
         'D'  -> return '\^D'
         'E'  -> return '\^E'
         'F'  -> return '\^F'
         'G'  -> return '\^G'
         'H'  -> return '\^H'
         'I'  -> return '\^I'
         'J'  -> return '\^J'
         'K'  -> return '\^K'
         'L'  -> return '\^L'
         'M'  -> return '\^M'
         'N'  -> return '\^N'
         'O'  -> return '\^O'
         'P'  -> return '\^P'
         'Q'  -> return '\^Q'
         'R'  -> return '\^R'
         'S'  -> return '\^S'
         'T'  -> return '\^T'
         'U'  -> return '\^U'
         'V'  -> return '\^V'
         'W'  -> return '\^W'
         'X'  -> return '\^X'
         'Y'  -> return '\^Y'
         'Z'  -> return '\^Z'
         '['  -> return '\^['
         '\\' -> return '\^\'
         ']'  -> return '\^]'
         '^'  -> return '\^^'
         '_'  -> return '\^_'
         _    -> empty

  lexAscii =
    choice
                -- \SO and \SOH need maximal-munch treatment
                -- See the Haskell report Sect 2.6
         [ '\SOH' <$ string "SOH"
         , '\SO'  <$ string "SO"
         , '\NUL' <$ string "NUL"
         , '\STX' <$ string "STX"
         , '\ETX' <$ string "ETX"
         , '\EOT' <$ string "EOT"
         , '\ENQ' <$ string "ENQ"
         , '\ACK' <$ string "ACK"
         , '\BEL' <$ string "BEL"
         , '\BS'  <$ string "BS" 
         , '\HT'  <$ string "HT" 
         , '\LF'  <$ string "LF" 
         , '\VT'  <$ string "VT" 
         , '\FF'  <$ string "FF" 
         , '\CR'  <$ string "CR" 
         , '\SI'  <$ string "SI" 
         , '\DLE' <$ string "DLE"
         , '\DC1' <$ string "DC1"
         , '\DC2' <$ string "DC2"
         , '\DC3' <$ string "DC3"
         , '\DC4' <$ string "DC4"
         , '\NAK' <$ string "NAK"
         , '\SYN' <$ string "SYN"
         , '\ETB' <$ string "ETB"
         , '\CAN' <$ string "CAN"
         , '\EM'  <$ string "EM" 
         , '\SUB' <$ string "SUB"
         , '\ESC' <$ string "ESC"
         , '\FS'  <$ string "FS" 
         , '\GS'  <$ string "GS" 
         , '\RS'  <$ string "RS" 
         , '\US'  <$ string "US" 
         , '\SP'  <$ string "SP" 
         , '\DEL' <$ string "DEL"
         ]


-- | String literal.
litString :: Parser TL.Text
litString = char '"' *> body mempty
 where
  body b = do
    t <- A.takeWhile isPlainChar
    (c,esc) <- lexStrItem
    if c /= '"' || esc
    then body (b <> TB.fromText t <> TB.singleton c)
    else return $ TB.toLazyText (b <> TB.fromText t)

  isPlainChar c = c /= '\\' && c /= '"'

  lexStrItem = (lexEmpty >> lexStrItem)
               <|> lexCharE

  lexEmpty =
    do _ <- char '\\'
       c <- anyChar
       case c of
         '&'           -> do return ()
         _ | isSpace c -> do skipSpace; _ <- char '\\'; return ()
         _             -> do empty
