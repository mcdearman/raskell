module Syntax.Reader where

import Data.Text (Text)
import Data.Void (Void)
import Syntax.SExpr
import Text.Megaparsec
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

-- Symbols can be any sequence of characters that are not whitespace, parens, or quotes
symbolParser :: Parser Text
symbolParser =
  lexeme $ takeWhile1P (Just "symbol") (\c -> c `notElem` [' ', '\n', '\r', '\t', '(', ')', '\'', '`', ','])

keyword :: Parser Atom
keyword = lexeme (char ':' *> (Keyword <$> symbolParser))

atom :: Parser Atom
atom =
  choice
    [ Int <$> int,
      String <$> stringLiteral,
      keyword,
      Symbol <$> symbolParser
    ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

cons :: Parser SExpr
cons = Cons <$> sexpr <*> sexpr <*> pure (Span 0 0)

-- quote :: Parser SExpr
-- quote = char '\'' *> (Cons . (Atom (ASymbol "quote") :) . pure <$> sexpr)

-- quasiquote :: Parser SExpr
-- quasiquote = char '`' *> (SList . (SAtom (ASymbol "quasiquote") :) . pure <$> sexpr)

-- unquote :: Parser SExpr
-- unquote = char ',' *> (SList . (SAtom (ASymbol "unquote") :) . pure <$> sexpr)

-- unquoteSplicing :: Parser SExpr
-- unquoteSplicing = lexeme ",@" *> (SList . (SAtom (ASymbol "unquote-splicing") :) . pure <$> sexpr)

-- list :: Parser SExpr
-- list = SList <$> parens (many sexpr)
-- cons :: Parser SExpr
-- cons = Cons <$> sexpr <*> sexpr <*> pure (Span 0 0)

-- sexpr :: Parser SExpr
-- sexpr = choice [Atom <$> atom] -- list, quote, quasiquote, unquoteSplicing, unquote]

-- parseSExpr :: Text -> Either (ParseErrorBundle Text Void) SExpr
-- parseSExpr = parse sexpr ""