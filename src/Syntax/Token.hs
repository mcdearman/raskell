module Syntax.Token where

import Common.Span (Span)
import Data.Ratio (Ratio)
import Data.Text (Text)
import GHC.TypeLits (Nat, Natural)

data Token = Token
  { tokenType :: TokenType,
    span :: Span
  }
  deriving (Show, Eq)

data TokenType
  = Symbol Text
  | Keyword Text
  | Nat Nat
  | BigNat Natural
  | Int Int
  | BigInt Integer
  | Rational (Ratio Int)
  | BigRational Rational
  | Real Double
  | String Text
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
  | Eof
  deriving (Show, Eq)
