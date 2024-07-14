module Syntax.Sexpr where

import Common.Span (Span)

data Sexpr
  = Atom Atom Span
  | Cons Sexpr Sexpr Span
  deriving (Show, Eq)

data Atom
  = Symbol String
  | Keyword String
  | Int Int
  | String String
  deriving (Show, Eq)