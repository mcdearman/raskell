module Syntax.Sexpr where

import Common.Span (Span)

data Sexpr = Sexpr
  { sexprType :: SexprType,
    span :: Span
  }
  deriving (Show, Eq)

data SexprType
  = Atom Atom
  | Cons Sexpr Sexpr
  deriving (Show, Eq)

data Atom
  = Symbol String
  | Keyword String
  | Int Int
  | String String
  deriving (Show, Eq)