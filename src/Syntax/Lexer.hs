module Syntax.Lexer where

import Common.Span (Span)
import qualified Data.Text as T
import Syntax.Token (Token)

data Lexer = Lexer
  { source :: T.Text,
    position :: Int
  }
  deriving (Show, Eq)

data LexerError = LexerError
  { message :: T.Text,
    span :: Span
  }
  deriving (Show, Eq)

peek :: Lexer -> Maybe Char
peek (Lexer src pos) =
  if pos < (T.length src - 1)
    then Just (T.index src pos)
    else Nothing

bump :: Lexer -> Lexer
bump (Lexer src pos) = Lexer src (pos + 1)

lex :: T.Text -> Either LexerError [Token]
lex src = let lexer = Lexer src 0 in Right []