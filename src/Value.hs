module Value where

import Data.Text
import RuntimeException
import SExpr (SExpr, Spanned)

data Value
  = VInt Integer
  | VReal Double
  | VString Text
  | VSymbol Text
  | VKeyword Text
  | VList [Value]
  | VLambda [Text] Bool (Spanned SExpr)
  | VNativeFn NativeFn
  deriving (Eq, Show)

newtype NativeFn = NativeFn ([Value] -> Either RuntimeException Value)

instance Eq NativeFn where
  _ == _ = False

instance Show NativeFn where
  show _ = "<nativeFn>"