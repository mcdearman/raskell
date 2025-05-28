module Reader (readSExpr) where

import Control.Applicative (empty, (<|>))
import Data.Text (Text, pack)
import Data.Void
import SExpr
import Span
import Text.Megaparsec
  ( MonadParsec (notFollowedBy, try),
    ParseErrorBundle,
    Parsec,
    Stream,
    between,
    choice,
    getOffset,
    many,
    manyTill,
    parse,
    satisfy,
  )

type Parser = Parsec Void Text

withSpan :: Parser a -> Parser (Spanned a)
withSpan p = do
  startPos <- getOffset
  result <- p
  Spanned result . SrcLoc startPos <$> getOffset

atom :: Parser (Spanned Atom)
atom = undefined

-- choice
--   [ fmap AInt <$>
--   ]

-- parens :: Parser a -> Parser a
-- parens = between (symbol "(") (symbol ")")

-- quote :: Parser (Spanned SExpr)
-- quote = withSpan $ do
--   ch <- withSpan $ char '\''
--   expr <- sexpr
--   return $ SList [Spanned (SAtom (ASymbol "quote")) (SExpr.span ch), expr]

-- quasiquote :: Parser (Spanned SExpr)
-- quasiquote = withSpan $ do
--   ch <- withSpan $ char '`'
--   expr <- sexpr
--   return $ SList [Spanned (SAtom (ASymbol "quasiquote")) (SExpr.span ch), expr]

-- unquote :: Parser (Spanned SExpr)
-- unquote = withSpan $ do
--   ch <- withSpan $ char ','
--   expr <- sexpr
--   return $ SList [Spanned (SAtom (ASymbol "unquote")) (SExpr.span ch), expr]

-- unquoteSplicing :: Parser (Spanned SExpr)
-- unquoteSplicing = withSpan $ do
--   s <- withSpan $ symbol ",@"
--   expr <- sexpr
--   return $ SList [Spanned (SAtom (ASymbol "unquote-splicing")) (SExpr.span s), expr]

-- list :: Parser (Spanned SExpr)
-- list = withSpan (SList <$> parens (many sexpr))

-- sexpr :: Parser (Spanned SExpr)
-- sexpr =
--   sc
--     *> choice
--       [ fmap SAtom <$> atom,
--         list,
--         quote,
--         quasiquote,
--         unquoteSplicing,
--         unquote
--       ]

readSExpr :: Text -> Either (ParseErrorBundle Text Void) (Spanned SExpr)
readSExpr = undefined

-- readSExpr = parse sexpr ""
