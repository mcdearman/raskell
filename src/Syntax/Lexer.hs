module Syntax.Lexer where

import Common.Span (Span)
import Data.Text (Text)
import Syntax.Token (Token)

data Lexer = Lexer
  { source :: String,
    position :: Int
  }
  deriving (Show, Eq)

data LexerError = LexerError
  { message :: Text,
    span :: Span
  }
  deriving (Show, Eq)

peek :: Lexer -> Maybe Char
peek (Lexer src pos) =
  if pos < length src then Just (src !! pos) else Nothing

bump :: Lexer -> Lexer
bump (Lexer src pos) = Lexer src (pos + 1)

lex :: String -> Either LexerError [Token]
lex src = let lexer = Lexer src 0 in Right []