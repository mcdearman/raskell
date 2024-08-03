module SExpr where

import Data.Text (Text, pack, unpack)
import RuntimeException (RuntimeException)

-- Data structure to hold span information
data Span = Span
  { start :: Int,
    end :: Int
  }
  deriving (Show, Eq)

-- Data structure to hold a parsed value along with its span
data Spanned a = Spanned
  { value :: a,
    span :: Span
  }
  deriving (Show, Eq)

data SExpr
  = SAtom Atom
  | SList [Spanned SExpr]
  | SLambda [Spanned Text] Bool (Spanned SExpr)
  | SNativeFn NativeFn
  deriving (Eq, Show)

prettySExpr :: Spanned SExpr -> Text
prettySExpr (Spanned (SAtom x) s) = prettyAtom x <> " @ " <> pack (show s)
prettySExpr (Spanned (SList xs) s) = "(" <> pack (unwords $ map (unpack . prettySExpr) xs) <> ") @ " <> pack (show s)
prettySExpr (Spanned (SLambda {}) s) = "<lambda> @ " <> pack (show s)
prettySExpr (Spanned (SNativeFn _) s) = "<nativeFn> @ " <> pack (show s)

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
