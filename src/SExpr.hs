module SExpr where

import Data.Text (Text, pack, unpack)
import Span
import Prelude hiding (span)

data SExpr
  = SAtom Atom
  | SList [Spanned SExpr]
  deriving (Eq, Show)

prettySExpr :: SExpr -> Text
prettySExpr (SAtom x) = prettyAtom x
prettySExpr (SList xs) = "(" <> pack (unwords $ map (unpack . prettySExpr . value) xs) <> ")"

data Atom
  = AInt Int
  | AReal Double
  | AString Text
  | ASymbol Text
  | AKeyword Text
  deriving (Eq, Show)

prettyAtom :: Atom -> Text
prettyAtom (AInt x) = pack $ show x
prettyAtom (AReal x) = pack $ show x
prettyAtom (AString x) = pack $ show x
prettyAtom (ASymbol x) = x
prettyAtom (AKeyword x) = ":" <> x
