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

prettyValue :: Value -> Text
prettyValue (VInt i) = pack $ show i
prettyValue (VReal r) = pack $ show r
prettyValue (VString s) = s
prettyValue (VSymbol s) = s
prettyValue (VKeyword s) = ":" <> s
prettyValue (VList l) = "(" <> intercalate " " (prettyValue <$> l) <> ")"
prettyValue (VLambda _ _ _) = "<lambda>"
prettyValue (VNativeFn _) = "<nativeFn>"