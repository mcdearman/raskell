module Parser where

import Data.Text (Text, pack)
import Data.Void
import SExpr
import Text.Megaparsec
  ( MonadParsec (try),
    ParseErrorBundle,
    Parsec,
    between,
    choice,
    empty,
    many,
    manyTill,
    oneOf,
    parse,
  )
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

int :: Parser Integer
int = lexeme L.decimal

signedInt :: Parser Integer
signedInt = L.signed sc int

real :: Parser Double
real = lexeme L.float

num :: Parser Number
num = try (NReal <$> real) <|> try (NInt <$> signedInt)

-- Symbols must start with an alphabetic character or one of the following:
-- + - * / = < > ! ? $ % & ^ _ ~
-- The remaining characters can be any sequence of
-- characters that are not whitespace, parens, or quotes.

symbolParser :: Parser Text
symbolParser =
  lexeme $
    pack
      <$> ( (:)
              <$> (letterChar <|> oneOf ['+', '-', '*', '/', '=', '<', '>', '!', '?', '$', '%', '&', '^', '_', '~'])
              <*> many (alphaNumChar <|> oneOf ['+', '-', '*', '/', '=', '<', '>', '!', '?', '$', '%', '&', '^', '_', '~'])
          )

-- symbolParser :: Parser Text
-- symbolParser =
--   lexeme $ takeWhile1P (Just "symbol") (\c -> c `notElem` [' ', '\n', '\r', '\t', '(', ')', '\'', '`', ','])

keyword :: Parser Atom
keyword = lexeme (char ':' *> (AKeyword <$> symbolParser))

atom :: Parser Atom
atom =
  choice
    [ ANum <$> num,
      AString <$> stringLiteral,
      keyword,
      ASymbol <$> symbolParser
    ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

quote :: Parser SExpr
quote = char '\'' *> (SList . (SAtom (ASymbol "quote") :) . pure <$> sexpr)

quasiquote :: Parser SExpr
quasiquote = char '`' *> (SList . (SAtom (ASymbol "quasiquote") :) . pure <$> sexpr)

unquote :: Parser SExpr
unquote = char ',' *> (SList . (SAtom (ASymbol "unquote") :) . pure <$> sexpr)

unquoteSplicing :: Parser SExpr
unquoteSplicing = lexeme ",@" *> (SList . (SAtom (ASymbol "unquote-splicing") :) . pure <$> sexpr)

list :: Parser SExpr
list = SList <$> parens (Text.Megaparsec.many sexpr)

sexpr :: Parser SExpr
sexpr = choice [SAtom <$> atom, list, quote, quasiquote, unquoteSplicing, unquote]

parseSExpr :: Text -> Either (ParseErrorBundle Text Void) SExpr
parseSExpr = parse sexpr ""