module Syntax.SExpr where

import Common.Span (Span)
import Data.Text

data SExpr
  = Atom Atom Span
  | Cons SExpr SExpr Span
  deriving (Show, Eq)

data Atom
  = Symbol Text
  | Keyword Text
  | Int Integer
  | String String
  deriving (Show, Eq)
