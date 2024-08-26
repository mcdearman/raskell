module Token where

import Data.Text (Text)
import Span
import Text.Megaparsec (Stream (Token, Tokens))

data Token
  = TVar Text
  | TInt Int
  | TRational Rational
  | TReal Double
  | TString Text
  | TLParen
  | TRParen
  | TPeriod
  | TComma
  | TApostrophe
  | TBacktick
  | TWhitespace
  | TComment
  | TEOF

data TokenStream = TokenStream
  { src :: Text,
    tokens :: [Spanned Token.Token]
  }

instance Stream TokenStream where
  type Token TokenStream = Spanned Token.Token
  type Tokens TokenStream = TokenStream

--   tokensToChunk _ = id
--   chunkToTokens _ = id
--   chunkLength _ = length
--   chunkEmpty _ = null
--   take1_ (TokenStream _ []) = Nothing
--   take1_ (TokenStream src (t : ts)) = Just (t, TokenStream src ts)
--   takeN_ n (TokenStream src ts) = (take n ts, TokenStream src (drop n ts))
--   takeWhile_ f (TokenStream src ts) = (prefix, TokenStream src suffix)
--     where
--       (prefix, suffix) = span f ts
--   showTokens _ = unwords . map (show . value)