module SExpr where

import Data.Text (Text, pack, unpack)
import RuntimeException (RuntimeException)

data SExpr
  = SAtom Atom
  | SList [SExpr]
  | SLambda [Text] Bool SExpr
  | SNativeFn NativeFn
  deriving (Eq, Show)

prettySExpr :: SExpr -> Text
prettySExpr (SAtom x) = prettyAtom x
prettySExpr (SList xs) = "(" <> pack (unwords $ map (unpack . prettySExpr) xs) <> ")"
prettySExpr (SLambda {}) = "<lambda>"
prettySExpr (SNativeFn _) = "<nativeFn>"

newtype NativeFn = NativeFn ([SExpr] -> Either RuntimeException SExpr)

instance Eq NativeFn where
  _ == _ = False

instance Show NativeFn where
  show _ = "<nativeFn>"

data Atom
  = AInt Integer
  | AString String
  | ASymbol Text
  | AKeyword Text
  deriving (Eq, Show)

prettyAtom :: Atom -> Text
prettyAtom (AInt x) = pack $ show x
prettyAtom (AString x) = pack $ show x
prettyAtom (ASymbol x) = x
prettyAtom (AKeyword x) = ":" <> x
