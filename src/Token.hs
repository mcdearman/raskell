{-# LANGUAGE TypeFamilies #-}

module Token where

import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Text (Text)
import Span
import Text.Megaparsec (Stream)
import qualified Text.Megaparsec as M

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
  deriving (Show, Eq, Ord)

data TokenStream = TokenStream
  { src :: Text,
    tokens :: [Spanned Token]
  }

instance Stream TokenStream where
  type Token TokenStream = Spanned Token
  type Tokens TokenStream = [Spanned Token]

  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy = id

  chunkLength _ = length
  chunkEmpty _ = null
  take1_ (TokenStream _ []) = Nothing
  take1_ (TokenStream str (t : ts)) =
    Just
      ( t,
        TokenStream str ts
      )

--   takeN_ n (TokenStream str ts)
--     | n <= 0 = Just ([], TokenStream str ts)
--     | null ts = Nothing
--     | otherwise =
--         let (x, ts') = splitAt n ts
--          in case NE.nonEmpty x of
--               Nothing -> Just (x, TokenStream str ts')
--               Just nex -> Just (x, TokenStream (drop (tokensLength pxy nex) str) s')
