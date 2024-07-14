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
  | Int
  | String
  | Whitespace
  | Comment
  deriving (Show, Eq)
