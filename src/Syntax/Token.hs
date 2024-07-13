module Syntax.Token where

import Common.Span (Span)

data Token = Token {tokenType :: TokenType, span :: Span} deriving (Show, Eq)

data TokenType
  = Ident
  | Num
  | String
  | Keyword
  | Whitespace
  | Comment
  deriving (Show, Eq)
