module Syntax.Lexer where

import Common.Span (Span)
import qualified Data.Text as T
import Syntax.Token (Token)

data Lexer = Lexer
  { source :: T.Text,
    position :: Int,
    tokens :: [Token]
  }
  deriving (Show, Eq)

data LexerError = LexerError
  { message :: T.Text,
    span :: Span
  }
  deriving (Show, Eq)

peek :: Lexer -> Maybe Char
peek (Lexer src pos _) =
  if pos < (T.length src - 1)
    then Just (T.index src pos)
    else Nothing

bump :: Lexer -> Lexer
bump (Lexer src pos ts) = Lexer src (pos + 1) ts

lex :: Lexer -> Either LexerError Lexer
lex lexer@(Lexer src 0 tokens) = Right lexer
lex lexer@(Lexer src pos tokens) = case peek lexer of
  Just c -> 
  Nothing -> Right lexer
