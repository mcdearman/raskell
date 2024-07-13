module Token where

data Token = Token {tokenType :: TokenType, span :: Span} deriving (Show, Eq)

data TokenType
  = Ident
  | Num
  | String
  | Keyword
  | Whitespace
  | Comment
  deriving (Show, Eq)
