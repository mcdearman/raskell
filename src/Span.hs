module Span where

import Data.Text (Text, pack)

data Span
  = SrcLoc {start :: Int, end :: Int}
  | Gen Span
  | NoLoc
  deriving (Show, Eq, Ord)

instance Semigroup Span where
  SrcLoc s1 e1 <> SrcLoc s2 e2 = SrcLoc (min s1 s2) (max e1 e2)
  Gen s1 <> Gen s2 = Gen (s1 <> s2)
  Gen s <> s' = Gen (s <> s')
  s <> Gen s' = Gen (s <> s')
  _ <> _ = NoLoc

data Spanned a = Spanned
  { value :: a,
    span :: Span
  }
  deriving (Show, Eq, Ord)

prettySpanned :: (Show a) => Spanned a -> Text
prettySpanned (Spanned val (SrcLoc s e)) = pack $ show val ++ " @ " ++ show s ++ ".." ++ show e
prettySpanned (Spanned val (Gen s)) = pack $ show val ++ " @ Gen" ++ show s
prettySpanned (Spanned val NoLoc) = pack $ show val ++ " @ NoLoc"

instance Functor Spanned where
  fmap f (Spanned v s) = Spanned (f v) s
