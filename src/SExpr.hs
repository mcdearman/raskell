module SExpr where

import Data.Text (Text, pack, unpack)
import RuntimeException (RuntimeException)
import Prelude hiding (span)

-- Data structure to hold span information
data Span = Span
  { start :: Int,
    end :: Int
  }
  deriving (Show, Eq)

data Spanned a = Spanned
  { value :: a,
    span :: Span
  }
  deriving (Show, Eq)

prettySpanned :: (Show a) => Spanned a -> Text
prettySpanned (Spanned val (Span s e)) = pack $ show val ++ " @ " ++ show s ++ ".." ++ show e

instance Functor Spanned where
  fmap f (Spanned v s) = Spanned (f v) s

data SExpr
  = SAtom Atom
  | SList [Spanned SExpr]
  deriving (Eq, Show)

prettySExpr :: SExpr -> Text
prettySExpr (SAtom x) = prettyAtom x
prettySExpr (SList xs) = "(" <> pack (unwords $ map (unpack . prettySExpr . value) xs) <> ")"

data Atom
  = AInt Integer
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
