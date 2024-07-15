module Syntax.Token where

import Common.Span (Span)

data Token = Token
  { tokenType :: TokenType,
    span :: Span
  }
  deriving (Show, Eq)

data TokenType
  = Symbol
  | Keyword
  | Nat
  | Int
  | Rational
  | Real
  | String
  | LParen
  | RParen
  | LBracket
  | RBracket
  | LBrace
  | RBrace
  | Quote
  | Backquote
  | Comma
  | CommaAt
  | Whitespace
  | Comment
  deriving (Show, Eq)
