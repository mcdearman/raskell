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

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

scWithSpan :: Parser (Spanned ())
scWithSpan = withSpan sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lexemeWithSpan :: Parser a -> Parser (Spanned a)
lexemeWithSpan = withSpan . lexeme

symbol :: Text -> Parser Text
symbol = L.symbol sc

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

octal :: Parser Integer
octal = char '0' >> char' 'o' >> L.octal

hexadecimal :: Parser Integer
hexadecimal = char '0' >> char' 'x' >> L.hexadecimal

int :: Parser Integer
int = lexeme (try octal <|> try hexadecimal <|> try L.decimal)

signedInt :: Parser Integer
signedInt = lexeme $ L.signed (notFollowedBy space1) int

real :: Parser Double
real = lexeme $ L.signed (notFollowedBy space1) L.float

-- Symbols must start with an alphabetic character or one of the following:
-- + - * / = < > ! ? $ % & ^ _ ~
-- The remaining characters can be any sequence of
-- characters that are not whitespace, parens, or quotes.

readSymbol :: Parser Text
readSymbol =
  lexeme $ pack <$> ((:) <$> symbolStartChar <*> many symbolChar)
  where
    symbolStartChar = letterChar <|> satisfy (`elem` ("+-*/=<>!?$%&^_~" :: String))
    symbolChar = alphaNumChar <|> satisfy (`notElem` (" \n\t\r()'`," :: String))

keyword :: Parser Atom
keyword = lexeme (char ':' *> (AKeyword <$> readSymbol))

atom :: Parser Atom
atom =
  choice
    [ try (AReal <$> real) <|> try (AInt <$> signedInt) <|> try (ASymbol <$> readSymbol),
      AString <$> stringLiteral,
      keyword
    ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

quote :: Parser (Spanned SExpr)
quote = withSpan $ do
  ch <- withSpan $ char '\''
  expr <- sexpr
  return $ SList [Spanned (SAtom (ASymbol "quote")) (SExpr.span ch), expr]

-- quasiquote :: Parser (Spanned SExpr)
-- quasiquote = char '`' *> (SList . (SAtom (ASymbol "quasiquote") :) . pure <$> sexpr)
quasiquote :: Parser (Spanned SExpr)
quasiquote = withSpan $ do
  ch <- withSpan $ char '`'
  expr <- sexpr
  return $ SList [Spanned (SAtom (ASymbol "quasiquote")) (SExpr.span ch), expr]

-- unquote :: Parser (Spanned SExpr)
-- unquote = char ',' *> (SList . (SAtom (ASymbol "unquote") :) . pure <$> sexpr)
unquote :: Parser (Spanned SExpr)
unquote = withSpan $ do
  ch <- withSpan $ char '`'
  expr <- sexpr
  return $ SList [Spanned (SAtom (ASymbol "unquote")) (SExpr.span ch), expr]

-- unquoteSplicing :: Parser (Spanned SExpr)
-- unquoteSplicing = lexeme ",@" *> (SList . (SAtom (ASymbol "unquote-splicing") :) . pure <$> sexpr)
unquoteSplicing :: Parser (Spanned SExpr)
unquoteSplicing = withSpan $ do
  ch <- withSpan $ char '`'
  expr <- sexpr
  return $ SList [Spanned (SAtom (ASymbol "unquote-splicing")) (SExpr.span ch), expr]

list :: Parser (Spanned SExpr)
list = withSpan (SList <$> parens (many sexpr))

sexpr :: Parser (Spanned SExpr)
sexpr =
  choice
    [ withSpan (SAtom <$> atom),
      list,
      quote,
      quasiquote,
      unquoteSplicing,
      unquote
    ]

readSExpr :: Text -> Either (ParseErrorBundle Text Void) (Spanned SExpr)
readSExpr = parse sexpr ""
