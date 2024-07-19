{
module Syntax.Lexer where

import Data.Text (Text)
import Data.Ratio (Ratio)
import GHC.TypeLits (Nat, Natural)
import Common.Span (Span)
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  $white+                        ;
  ";".*                         ;
  $digit+                        { \s -> Int (read s) }
  $alpha [$alpha $digit \_ \']*  { \s -> Symbol s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token = {
    kind :: TokenKind,
    span :: Span
}

data TokenKind
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
}