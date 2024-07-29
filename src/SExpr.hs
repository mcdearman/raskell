module SExpr where

import Data.Ratio
import Data.Text (Text, pack, unpack)
import RuntimeException (RuntimeException)

data SExpr
  = SAtom Atom
  | SList [SExpr]
  | Cons SExpr SExpr
  | SLambda [Text] Bool SExpr
  | SNativeFn NativeFn
  deriving (Eq, Show)

prettySExpr :: SExpr -> Text
prettySExpr (SAtom x) = prettyAtom x
prettySExpr (SList xs) = "(" <> pack (unwords $ map (unpack . prettySExpr) xs) <> ")"
prettySExpr (Cons x y) = "(" <> prettySExpr x <> " . " <> prettySExpr y <> ")"
prettySExpr (SLambda {}) = "<lambda>"
prettySExpr (SNativeFn _) = "<nativeFn>"

newtype NativeFn = NativeFn ([SExpr] -> Either RuntimeException SExpr)

instance Eq NativeFn where
  _ == _ = False

instance Show NativeFn where
  show _ = "<nativeFn>"

data Atom
  = ANum Number
  | AString String
  | ASymbol Text
  | AKeyword Text
  deriving (Eq, Show)

prettyAtom :: Atom -> Text
prettyAtom (ANum x) = prettyNumber x
prettyAtom (AString x) = pack $ show x
prettyAtom (ASymbol x) = x
prettyAtom (AKeyword x) = ":" <> x

data Number
  = NInt Integer
  | NRational Rational
  | NReal Double
  deriving (Eq, Show)

instance Num Number where
  NInt a + NInt b = NInt (a + b)
  NInt a + NReal b = NReal (fromIntegral a + b)
  NInt a + NRational b = NRational (fromIntegral a + b)
  NReal a + NInt b = NReal (a + fromIntegral b)
  NReal a + NReal b = NReal (a + b)
  NReal a + NRational b = NReal (a + fromRational b)
  NRational a + NInt b = NRational (a + fromIntegral b)
  NRational a + NReal b = NReal (fromRational a + b)
  NRational a + NRational b = NRational (a + b)
  NInt a - NInt b = NInt (a - b)
  NInt a - NReal b = NReal (fromIntegral a - b)
  NInt a - NRational b = NRational (fromIntegral a - b)
  NReal a - NInt b = NReal (a - fromIntegral b)
  NReal a - NReal b = NReal (a - b)
  NReal a - NRational b = NReal (a - fromRational b)
  NRational a - NInt b = NRational (a - fromIntegral b)
  NRational a - NReal b = NReal (fromRational a - b)
  NRational a - NRational b = NRational (a - b)
  NInt a * NInt b = NInt (a * b)
  NInt a * NReal b = NReal (fromIntegral a * b)
  NInt a * NRational b = NRational (fromIntegral a * b)
  NReal a * NInt b = NReal (a * fromIntegral b)
  NReal a * NReal b = NReal (a * b)
  NReal a * NRational b = NReal (a * fromRational b)
  NRational a * NInt b = NRational (a * fromIntegral b)
  NRational a * NReal b = NReal (fromRational a * b)
  NRational a * NRational b = NRational (a * b)
  abs (NInt a) = NInt (abs a)
  abs (NReal a) = NReal (abs a)
  abs (NRational a) = NRational (abs a)
  signum (NInt a) = NInt (signum a)
  signum (NReal a) = NReal (signum a)
  signum (NRational a) = NRational (signum a)
  fromInteger = NInt
  negate (NInt a) = NInt (negate a)
  negate (NReal a) = NReal (negate a)
  negate (NRational a) = NRational (negate a)

instance Fractional Number where
  NInt a / NInt b = NRational (a % b)
  NInt a / NReal b = NReal (fromIntegral a / b)
  NInt a / NRational b = NReal (fromIntegral a / fromRational b)
  NReal a / NInt b = NReal (a / fromIntegral b)
  NReal a / NReal b = NReal (a / b)
  NReal a / NRational b = NReal (a / fromRational b)
  NRational a / NInt b = NRational (a / fromIntegral b)
  NRational a / NReal b = NReal (fromRational a / b)
  NRational a / NRational b = NReal (fromRational a / fromRational b)
  fromRational = NRational

prettyNumber :: Number -> Text
prettyNumber (NInt x) = pack $ show x
prettyNumber (NRational x) = pack $ show x
prettyNumber (NReal x) = pack $ show x
