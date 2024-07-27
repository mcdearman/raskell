module Main (main) where

import Control.Applicative
import Data.Ratio
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import Data.Void
import Debug.Trace
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Text.Megaparsec
  ( MonadParsec (takeWhile1P, try),
    ParseErrorBundle,
    Parsec,
    between,
    choice,
    empty,
    many,
    manyTill,
    parse,
  )
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Pretty.Simple (pPrint, pShow)

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
  = Int Integer
  | Rational Rational
  | Real Double
  deriving (Eq, Show)

instance Num Number where
  Int a + Int b = Int (a + b)
  Int a + Real b = Real (fromIntegral a + b)
  Int a + Rational b = Rational (fromIntegral a + b)
  Real a + Int b = Real (a + fromIntegral b)
  Real a + Real b = Real (a + b)
  Real a + Rational b = Real (a + fromRational b)
  Rational a + Int b = Rational (a + fromIntegral b)
  Rational a + Real b = Real (fromRational a + b)
  Rational a + Rational b = Rational (a + b)
  Int a - Int b = Int (a - b)
  Int a - Real b = Real (fromIntegral a - b)
  Int a - Rational b = Rational (fromIntegral a - b)
  Real a - Int b = Real (a - fromIntegral b)
  Real a - Real b = Real (a - b)
  Real a - Rational b = Real (a - fromRational b)
  Rational a - Int b = Rational (a - fromIntegral b)
  Rational a - Real b = Real (fromRational a - b)
  Rational a - Rational b = Rational (a - b)
  Int a * Int b = Int (a * b)
  Int a * Real b = Real (fromIntegral a * b)
  Int a * Rational b = Rational (fromIntegral a * b)
  Real a * Int b = Real (a * fromIntegral b)
  Real a * Real b = Real (a * b)
  Real a * Rational b = Real (a * fromRational b)
  Rational a * Int b = Rational (a * fromIntegral b)
  Rational a * Real b = Real (fromRational a * b)
  Rational a * Rational b = Rational (a * b)
  abs (Int a) = Int (abs a)
  abs (Real a) = Real (abs a)
  abs (Rational a) = Rational (abs a)
  signum (Int a) = Int (signum a)
  signum (Real a) = Real (signum a)
  signum (Rational a) = Rational (signum a)
  fromInteger = Int
  negate (Int a) = Int (negate a)
  negate (Real a) = Real (negate a)
  negate (Rational a) = Rational (negate a)

instance Fractional Number where
  Int a / Int b = Rational (a % b)
  Int a / Real b = Real (fromIntegral a / b)
  Int a / Rational b = Real (fromIntegral a / fromRational b)
  Real a / Int b = Real (a / fromIntegral b)
  Real a / Real b = Real (a / b)
  Real a / Rational b = Real (a / fromRational b)
  Rational a / Int b = Rational (a / fromIntegral b)
  Rational a / Real b = Real (fromRational a / b)
  Rational a / Rational b = Real (fromRational a / fromRational b)
  fromRational = Rational

instance Integral Number where
  toInteger (Int a) = a
  toInteger (Real a) = truncate a
  toInteger (Rational a) = truncate (fromRational a)

prettyNumber :: Number -> Text
prettyNumber (Int x) = pack $ show x
prettyNumber (Rational x) = pack $ show x
prettyNumber (Real x) = pack $ show x

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

int :: Parser Integer
int = lexeme L.decimal

signedInt :: Parser Integer
signedInt = L.signed sc int

real :: Parser Double
real = lexeme L.float

num :: Parser Number
num = try (Int <$> signedInt) <|> try (Real <$> real)

-- Symbols can be any sequence of characters that are not whitespace, parens, or quotes
symbolParser :: Parser Text
symbolParser =
  lexeme $ takeWhile1P (Just "symbol") (\c -> c `notElem` [' ', '\n', '\r', '\t', '(', ')', '\'', '`', ','])

keyword :: Parser Atom
keyword = lexeme (char ':' *> (AKeyword <$> symbolParser))

atom :: Parser Atom
atom =
  choice
    [ -- AReal <$> real,
      ANum <$> num,
      AString <$> stringLiteral,
      keyword,
      ASymbol <$> symbolParser
    ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

quote :: Parser SExpr
quote = char '\'' *> (SList . (SAtom (ASymbol "quote") :) . pure <$> sexpr)

quasiquote :: Parser SExpr
quasiquote = char '`' *> (SList . (SAtom (ASymbol "quasiquote") :) . pure <$> sexpr)

unquote :: Parser SExpr
unquote = char ',' *> (SList . (SAtom (ASymbol "unquote") :) . pure <$> sexpr)

unquoteSplicing :: Parser SExpr
unquoteSplicing = lexeme ",@" *> (SList . (SAtom (ASymbol "unquote-splicing") :) . pure <$> sexpr)

list :: Parser SExpr
list = SList <$> parens (Text.Megaparsec.many sexpr)

sexpr :: Parser SExpr
sexpr = choice [SAtom <$> atom, list, quote, quasiquote, unquoteSplicing, unquote]

parseSExpr :: Text -> Either (ParseErrorBundle Text Void) SExpr
parseSExpr = parse sexpr ""

type Env = [(Text, SExpr)]

newtype RuntimeException = RuntimeException Text

instance Show RuntimeException where
  show (RuntimeException msg) = "Runtime error: " ++ unpack msg

defaultEnv :: Env
defaultEnv =
  [ ( "+",
      SNativeFn $ NativeFn $ \case
        (SAtom (ANum a) : xs) -> Right $ SAtom $ ANum $ foldl (\acc (SAtom (ANum x)) -> acc + x) a xs
        [] -> Left $ RuntimeException "Must have at least one argument"
        _ -> Left $ RuntimeException "Arguments must be integers"
    ),
    ( "-",
      SNativeFn $ NativeFn $ \case
        [SAtom (ANum a), SAtom (ANum b)] -> Right $ SAtom $ ANum $ a - b
        _ -> Left $ RuntimeException "Arguments must be integers"
    ),
    ( "*",
      SNativeFn $ NativeFn $ \case
        [SAtom (ANum a), SAtom (ANum b)] -> Right $ SAtom $ ANum $ a * b
        _ -> Left $ RuntimeException "Arguments must be integers"
    ),
    ( "/",
      SNativeFn $ NativeFn $ \case
        [SAtom (ANum a), SAtom (ANum b)] -> Right $ SAtom $ ANum $ a `div` b
        _ -> Left $ RuntimeException "Arguments must be integers"
    ),
    ( "%",
      SNativeFn $ NativeFn $ \case
        [SAtom (ANum a), SAtom (ANum b)] -> Right $ SAtom $ ANum $ a `mod` b
        _ -> Left $ RuntimeException "Arguments must be integers"
    ),
    ( "pow",
      SNativeFn $ NativeFn $ \case
        [SAtom (ANum a), SAtom (ANum b)] -> Right $ SAtom $ ANum $ a ^ b
        _ -> Left $ RuntimeException "Arguments must be integers"
    ),
    ( "=",
      SNativeFn $ NativeFn $ \case
        [a, b] -> Right $ SAtom $ AKeyword $ if a == b then "t" else "f"
        _ -> Left $ RuntimeException "must have two arguments"
    ),
    ( "neq",
      SNativeFn $ NativeFn $ \case
        [a, b] -> Right $ SAtom $ AKeyword $ if a /= b then "t" else "f"
        _ -> Left $ RuntimeException "must have two arguments"
    ),
    ( ">",
      SNativeFn $ NativeFn $ \case
        [SAtom (ANum a), SAtom (ANum b)] -> Right $ SAtom $ AKeyword $ if a > b then "t" else "f"
        _ -> Left $ RuntimeException "Arguments must be integers"
    ),
    ( ">=",
      SNativeFn $ NativeFn $ \case
        [SAtom (ANum a), SAtom (ANum b)] -> Right $ SAtom $ AKeyword $ if a >= b then "t" else "f"
        _ -> Left $ RuntimeException "Arguments must be integers"
    ),
    ( "<",
      SNativeFn $ NativeFn $ \case
        [SAtom (ANum a), SAtom (ANum b)] -> Right $ SAtom $ AKeyword $ if a < b then "t" else "f"
        _ -> Left $ RuntimeException "Arguments must be integers"
    ),
    ( "<=",
      SNativeFn $ NativeFn $ \case
        [SAtom (ANum a), SAtom (ANum b)] -> Right $ SAtom $ AKeyword $ if a <= b then "t" else "f"
        _ -> Left $ RuntimeException "Arguments must be integers"
    ),
    ( "not",
      SNativeFn $ NativeFn $ \case
        [SAtom (AKeyword "t")] -> Right $ SAtom $ AKeyword "f"
        [SAtom (AKeyword "f")] -> Right $ SAtom $ AKeyword "t"
        _ -> Left $ RuntimeException "Argument must be a boolean"
    ),
    ( "empty?",
      SNativeFn $ NativeFn $ \case
        [SList []] -> Right $ SAtom $ AKeyword "t"
        [SList _] -> Right $ SAtom $ AKeyword "f"
        _ -> Left $ RuntimeException "Argument must be a list"
    ),
    ( "pair",
      SNativeFn $ NativeFn $ \case
        [a, SList b] -> Right $ SList (a : b)
        _ -> Left $ RuntimeException "Arguments must be a value and a list"
    ),
    ( "head",
      SNativeFn $ NativeFn $ \case
        [SList (x : _)] -> Right x
        _ -> Left $ RuntimeException "Argument must be a non-empty list"
    ),
    ( "tail",
      SNativeFn $ NativeFn $ \case
        [SList (_ : xs)] -> Right $ SList xs
        _ -> Left $ RuntimeException "Argument must be a non-empty list"
    ),
    ( "++",
      SNativeFn $ NativeFn $ \case
        [SList xs, SList ys] -> Right $ SList (xs ++ ys)
        _ -> Left $ RuntimeException "Arguments must be lists"
    ),
    ( "eval",
      SNativeFn $ NativeFn $ \case
        [x] -> eval x defaultEnv >>= \(result, _) -> Right result
        _ -> Left $ RuntimeException "Must have exactly one argument"
    )
  ]

expandMacro :: SExpr -> Env -> Either RuntimeException SExpr
expandMacro (SList (SAtom (ASymbol name) : args)) env = case lookup name env of
  Just (SLambda params True body) -> do
    eargs <- mapM (`expandMacro` env) args
    let newEnv = zip params eargs ++ env
    eval body newEnv >>= \(result, _) -> Right result
  _ -> Left $ RuntimeException "Not a macro"
expandMacro _ _ = Left $ RuntimeException "Invalid macro call"

eval :: SExpr -> Env -> Either RuntimeException (SExpr, Env)
eval (SAtom (ANum x)) env = Right (SAtom $ ANum x, env)
eval (SAtom (AString x)) env = Right (SAtom $ AString x, env)
eval (SAtom (AKeyword x)) env = Right (SAtom $ AKeyword x, env)
eval (SList [SAtom (ASymbol "def"), SAtom (ASymbol name), value]) env = do
  (value', _) <- eval value env
  Right (value', (name, value') : env)
eval (SList [SAtom (ASymbol "def"), SList (SAtom (ASymbol name) : params), body]) env =
  let lam = SLambda (map (\case (SAtom (ASymbol x)) -> x; _ -> error "Non-symbol function param") params) False body
   in Right (lam, (name, lam) : env)
eval (SList [SAtom (ASymbol "macro"), SList (SAtom (ASymbol name) : params), body]) env = do
  let lam = SLambda (map (\case (SAtom (ASymbol x)) -> x; _ -> error "Non-symbol function param") params) True body
   in Right (lam, (name, lam) : env)
eval (SList [SAtom (ASymbol "quote"), x]) env = Right (x, env)
eval (SList [SAtom (ASymbol "quasiquote"), x]) env =
  let recQuasiquote (SList [SAtom (ASymbol "unquote"), y]) = eval y env
      recQuasiquote (SList [SAtom (ASymbol "unquote-splicing"), y]) = do
        case eval y env of
          Right (SList ys, _) -> Right (SList (SAtom (ASymbol "++") : ys), env)
          _ -> Left $ RuntimeException "Unquote-splicing must evaluate to a list"
      recQuasiquote (SList ys) = do
        let splicedResults [] = Right []
            splicedResults (SList spliced : rest) = do
              rest' <- splicedResults rest
              Right (spliced ++ rest')
            splicedResults (y : rest) = do
              rest' <- splicedResults rest
              Right (y : rest')
        ys' <- mapM recQuasiquote ys
        let (results, _) = unzip ys'
        spliced <- splicedResults results
        trace ("spliced: " ++ unpack (toStrict (pShow spliced))) Right (SList spliced, env)
      recQuasiquote y = Right (y, env)
   in recQuasiquote x
eval (SList [SAtom (ASymbol "and"), a, b]) env = do
  (a', _) <- eval a env
  case a' of
    SAtom (AKeyword "t") -> eval b env
    SAtom (AKeyword "f") -> Right (SAtom $ AKeyword "f", env)
    _ -> Left $ RuntimeException "First argument must evaluate to a boolean"
eval (SList [SAtom (ASymbol "or"), a, b]) env = do
  (a', _) <- eval a env
  case a' of
    SAtom (AKeyword "t") -> Right (SAtom $ AKeyword "t", env)
    SAtom (AKeyword "f") -> eval b env
    _ -> Left $ RuntimeException "First argument must evaluate to a boolean"
eval (SList [SAtom (ASymbol "if"), cond, t, f]) env = do
  (cond', _) <- eval cond env
  case cond' of
    SAtom (AKeyword "t") -> eval t env
    SAtom (AKeyword "f") -> eval f env
    _ -> Left $ RuntimeException "Condition must evaluate to a boolean"
eval (SList [SAtom (ASymbol "let"), SList bindings, body]) env = do
  let evalBindings [] accEnv = Right accEnv
      evalBindings ((SList [SAtom (ASymbol name), value]) : rest) accEnv =
        case eval value accEnv of
          Right (value', _) -> evalBindings rest ((name, value') : accEnv)
          Left err -> Left err
      evalBindings _ _ = Left $ RuntimeException "Invalid let binding"
  case evalBindings bindings env of
    Right newEnv -> eval body newEnv
    Left err -> Left err
eval (SList [SAtom (ASymbol "fn"), SList params, body]) env = do
  Right (SLambda (map (\case (SAtom (ASymbol x)) -> x; _ -> error "Non-symbol fn param") params) False body, env)
eval (SList (SAtom (ASymbol "begin") : forms)) env = do
  let evalForms [] accEnv = Right (SList [], accEnv)
      evalForms [x] accEnv = eval x accEnv
      evalForms (x : xs) accEnv = do
        (_, newEnv) <- eval x accEnv
        evalForms xs newEnv
  case evalForms forms env of
    Right (result, _) -> Right (result, env)
    Left err -> Left err
eval (SList (SAtom (ASymbol "list") : xs)) env = do
  xs' <- mapM (`eval` env) xs
  Right (SList (map fst xs'), env)
eval (SAtom (ASymbol x)) env = case lookup x env of
  Just v -> Right (v, env)
  Nothing -> Left $ RuntimeException $ "Symbol '" <> x <> "' not found in environment"
eval sexpr@(SList (fn : args)) env = do
  (fn', fn_env) <- eval fn env
  args' <- mapM (fmap fst . (`eval` env)) args
  case fn' of
    SLambda params False body -> do
      let newEnv = zip params args' ++ fn_env
      eval body newEnv
    SLambda params True body -> do
      m <- trace "expansion: " (expandMacro sexpr fn_env)
      eval m env
    SNativeFn (NativeFn f) -> fmap (,fn_env) (f args')
    _ -> Left $ RuntimeException $ "First element of list must be a function got: " <> toStrict (pShow fn')
eval (SList []) _ = Left $ RuntimeException "Empty list"
eval e _ = Left $ RuntimeException ("Invalid expression: " <> pack (show e))

repl :: Env -> IO ()
repl env = do
  hSetBuffering stdout NoBuffering
  putStr "> "
  input <- pack <$> getLine
  case parseSExpr input of
    Left err -> pPrint err
    Right e -> do
      pPrint e
      case eval e env of
        Left err -> print err
        Right (result, env') -> do
          putStrLn $ unpack (prettySExpr result) ++ "\n"
          -- pPrint env'
          repl env'
  repl env

main :: IO ()
main = do
  putStrLn "Welcome to the Raskell REPL!"
  repl defaultEnv
