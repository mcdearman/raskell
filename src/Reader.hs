module Reader (readSExpr) where

import Control.Applicative (empty, (<|>))
import Data.Text (Text, pack)
import Data.Void
import SExpr
import Text.Megaparsec
  ( MonadParsec (notFollowedBy, try),
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

type Parser = Parsec Void Text

withSpan :: Parser a -> Parser (Spanned a)
withSpan p = do
  startPos <- getOffset
  result <- p
  Spanned result . Span startPos <$> getOffset

lexemeWithSpan :: Parser a -> Parser (Spanned a)
lexemeWithSpan p = withSpan p <* sc

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

symbol :: Text -> Parser (Spanned Text)
symbol p = lexemeWithSpan (L.symbol sc p)

stringLiteral :: Parser (Spanned Text)
stringLiteral = lexemeWithSpan $ char '\"' *> (pack <$> manyTill L.charLiteral (char '\"'))

octal :: Parser Integer
octal = char '0' >> char' 'o' >> L.octal

hexadecimal :: Parser Integer
hexadecimal = char '0' >> char' 'x' >> L.hexadecimal

int :: Parser Integer
int = try octal <|> try hexadecimal <|> L.decimal

signedInt :: Parser (Spanned Integer)
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

keyword :: Parser (Spanned Atom)
keyword = lexemeWithSpan (char ':' *> (AKeyword . value <$> readSymbol))

atom :: Parser (Spanned Atom)
atom =
  choice
    [ try (fmap AReal <$> real)
        <|> try (fmap AInt <$> signedInt)
        <|> try (fmap ASymbol <$> readSymbol),
      fmap AString <$> stringLiteral,
      keyword
    ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

quote :: Parser (Spanned SExpr)
quote = withSpan $ do
  ch <- withSpan $ char '\''
  expr <- sexpr
  return $ SList [Spanned (SAtom (ASymbol "quote")) (SExpr.span ch), expr]

quasiquote :: Parser (Spanned SExpr)
quasiquote = withSpan $ do
  ch <- withSpan $ char '`'
  expr <- sexpr
  return $ SList [Spanned (SAtom (ASymbol "quasiquote")) (SExpr.span ch), expr]

unquote :: Parser (Spanned SExpr)
unquote = withSpan $ do
  ch <- withSpan $ char ','
  expr <- sexpr
  return $ SList [Spanned (SAtom (ASymbol "unquote")) (SExpr.span ch), expr]

unquoteSplicing :: Parser (Spanned SExpr)
unquoteSplicing = withSpan $ do
  s <- withSpan $ symbol ",@"
  expr <- sexpr
  return $ SList [Spanned (SAtom (ASymbol "unquote-splicing")) (SExpr.span s), expr]

list :: Parser (Spanned SExpr)
list = withSpan (SList <$> parens (many sexpr))

sexpr :: Parser (Spanned SExpr)
sexpr =
  sc
    *> choice
      [ fmap SAtom <$> atom,
        list,
        quote,
        quasiquote,
        unquoteSplicing,
        unquote
      ]

readSExpr :: Text -> Either (ParseErrorBundle Text Void) (Spanned SExpr)
readSExpr = parse sexpr ""
