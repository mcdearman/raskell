module Syntax.SExpr where

import Common.Span (Span)
import Data.Text

data SExpr
  = Atom Atom Span
  | Cons SExpr SExpr Span
  deriving (Show, Eq)

textOfSExpr :: SExpr -> Text
textOfSExpr (Atom (Int x) _) = pack $ show x
textOfSExpr (Atom (Real x) _) = pack $ show x
textOfSExpr (Atom (String x) _) = pack $ show x
textOfSExpr (Atom (Symbol x) _) = x
textOfSExpr (Atom (Keyword x) _) = ":" <> x
-- textOfSExpr (SList xs) = "(" <> pack (unwords $ map (unpack . textOfSExpr) xs) <> ")"
textOfSExpr (Cons x y _) = "(" <> textOfSExpr x <> " . " <> textOfSExpr y <> ")"

-- textOfSExpr (SLambda {}) = "<lambda>"
-- textOfSExpr (SNativeFn _) = "<nativeFn>"

data Atom
  = Symbol Text
  | Keyword Text
  | Int Integer
  | Real Double
  | String String
  deriving (Show, Eq)
