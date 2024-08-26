module Lexer where

import Control.Applicative (empty, (<|>))
import Data.Text (Text, pack)
import Data.Void
import SExpr
import Span
import Text.Megaparsec
  ( MonadParsec (eof, notFollowedBy, try),
    ParseErrorBundle,
    Parsec,
    between,
    choice,
    getOffset,
    many,
    manyTill,
    parse,
    satisfy,
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    char',
    letterChar,
    space1,
  )
import qualified Text.Megaparsec.Char.Lexer as L
import Token

type Parser = Parsec Void Text

withSpan :: Parser a -> Parser (Spanned a)
withSpan p = do
  startPos <- getOffset
  result <- p
  Spanned result . SrcLoc startPos <$> getOffset

lexemeWithSpan :: Parser a -> Parser (Spanned a)
lexemeWithSpan p = withSpan p <* sc

sc :: Parser ()
sc = L.space space1 empty empty

symbol :: Text -> Parser (Spanned Text)
symbol p = lexemeWithSpan (L.symbol sc p)

stringLiteral :: Parser (Spanned Text)
stringLiteral = lexemeWithSpan $ char '\"' *> (pack <$> manyTill L.charLiteral (char '\"'))

octal :: Parser Int
octal = char '0' >> char' 'o' >> L.octal

hexadecimal :: Parser Int
hexadecimal = char '0' >> char' 'x' >> L.hexadecimal

int :: Parser Int
int = try octal <|> try hexadecimal <|> L.decimal

signedInt :: Parser (Spanned Int)
signedInt = lexemeWithSpan $ L.signed (notFollowedBy space1) int

real :: Parser (Spanned Double)
real = lexemeWithSpan $ L.signed (notFollowedBy space1) L.float

-- Symbols must start with an alphabetic character or one of the following:
-- + - * / = < > ! ? $ % & ^ _ ~
-- The remaining characters can be any sequence of
-- characters that are not whitespace, parens, or quotes.

readSymbol :: Parser (Spanned Text)
readSymbol =
  lexemeWithSpan $ pack <$> ((:) <$> symbolStartChar <*> many symbolChar)
  where
    symbolStartChar = try letterChar <|> satisfy (`elem` ("+-*/=<>!?$%&^_~" :: String))
    symbolChar = try alphaNumChar <|> satisfy (`notElem` (" \n\t\r()'`," :: String))

keyword :: Parser (Spanned Text)
keyword = lexemeWithSpan (char ':' *> (value <$> readSymbol))

token :: Parser (Spanned Token)
token =
  withSpan $
    choice
      [ TVar . value <$> readSymbol,
        TInt . value <$> signedInt,
        TRational . fromIntegral . value <$> signedInt,
        TReal . value <$> real,
        TString . value <$> stringLiteral,
        TLParen <$ symbol "(",
        TRParen <$ symbol ")",
        TPeriod <$ symbol ".",
        TComma <$ symbol ",",
        TApostrophe <$ symbol "'",
        TBacktick <$ symbol "`",
        TWhitespace <$ lexemeWithSpan (pack <$> many (satisfy (`elem` (" \n\t\r" :: String)))),
        TComment <$ lexemeWithSpan (char ';' *> manyTill L.charLiteral (char '\n')),
        TEOF <$ eof
      ]