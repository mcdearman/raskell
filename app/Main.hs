{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import Data.Text (Text, pack, unpack)
import Data.Void
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Text.Megaparsec
  ( MonadParsec (takeWhile1P),
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

data SExpr = SAtom Atom | SList [SExpr] | SLambda [Text] SExpr | SNativeFn NativeFn
  deriving (Eq, Show)

stringOfSExpr :: SExpr -> Text
stringOfSExpr (SAtom (AInt x)) = pack $ show x
stringOfSExpr (SAtom (AString x)) = pack $ show x
stringOfSExpr (SAtom (ASymbol x)) = x
stringOfSExpr (SList xs) = "(" <> pack (unwords $ map (unpack . stringOfSExpr) xs) <> ")"
stringOfSExpr (SLambda _ _) = "<lambda>"
stringOfSExpr (SNativeFn _) = "<nativeFn>"

newtype NativeFn = NativeFn ([SExpr] -> Either RuntimeException SExpr)

instance Eq NativeFn where
  _ == _ = False

instance Show NativeFn where
  show _ = "<nativeFn>"

data Atom = AInt Integer | AString String | ASymbol Text
  deriving (Eq, Show)

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

int :: Parser Integer
int = lexeme L.decimal

real :: Parser Double
real = lexeme L.float

isWhitespace :: Char -> Bool
isWhitespace c = c == ' ' || c == '\n' || c == '\r' || c == '\t'

-- Symbols can be any sequence of characters that are not whitespace, parens, or quotes
symbolParser :: Parser Text
symbolParser =
  lexeme $ takeWhile1P (Just "symbol") (\c -> not (isWhitespace c) && c /= '(' && c /= ')' && c /= '\'')

atom :: Parser Atom
atom =
  choice
    [ AInt <$> int,
      AString <$> stringLiteral,
      ASymbol <$> symbolParser
    ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

list :: Parser SExpr
list = SList <$> parens (many sexpr)

sexpr :: Parser SExpr
sexpr = choice [SAtom <$> atom, list]

parseSExpr :: Text -> Either (ParseErrorBundle Text Void) SExpr
parseSExpr = parse sexpr ""

type Env = [(Text, SExpr)]

newtype RuntimeException = RuntimeException Text deriving (Show)

defaultEnv :: Env
defaultEnv =
  [ ( "+",
      SNativeFn $ NativeFn $ \case
        (SAtom (AInt a) : xs) -> Right $ SAtom $ AInt $ foldl (\acc (SAtom (AInt x)) -> acc + x) a xs
        _ -> Left $ RuntimeException "Arguments must be integers"
    ),
    ( "-",
      SNativeFn $ NativeFn $ \case
        [SAtom (AInt a), SAtom (AInt b)] -> Right $ SAtom $ AInt $ a - b
        _ -> Left $ RuntimeException "Arguments must be integers"
    ),
    ( "*",
      SNativeFn $ NativeFn $ \case
        [SAtom (AInt a), SAtom (AInt b)] -> Right $ SAtom $ AInt $ a * b
        _ -> Left $ RuntimeException "Arguments must be integers"
    ),
    ( "/",
      SNativeFn $ NativeFn $ \case
        [SAtom (AInt a), SAtom (AInt b)] -> Right $ SAtom $ AInt $ a `div` b
        _ -> Left $ RuntimeException "Arguments must be integers"
    )
  ]

eval :: SExpr -> Env -> Either RuntimeException (SExpr, Env)
eval (SAtom (AInt x)) env = Right (SAtom $ AInt x, env)
eval (SAtom (AString x)) env = Right (SAtom $ AString x, env)
eval (SList [SAtom (ASymbol "def"), SAtom (ASymbol name), value]) env = do
  (value', _) <- eval value env
  Right (SAtom $ ASymbol name, (name, value') : env)
eval (SList [SAtom (ASymbol "quote"), x]) env = Right (x, env)
eval (SAtom (ASymbol x)) env = case lookup x env of
  Just v -> Right (v, env)
  Nothing -> Left $ RuntimeException $ "Symbol " <> x <> " not found in environment"
eval (SList (fn : args)) env = do
  (fn', fn_env) <- eval fn env
  args' <- mapM (fmap fst . (`eval` env)) args
  case fn' of
    SLambda params body -> do
      let newEnv = zip params args' ++ fn_env
      eval body newEnv
    SNativeFn (NativeFn f) -> fmap (,env) (f args')
    _ -> Left $ RuntimeException "First element of list must be a function"
eval (SList []) _ = Left $ RuntimeException "Empty list"
eval e _ = Left $ RuntimeException ("Invalid expression: " <> pack (show e))

repl :: Env -> IO ()
repl env = do
  hSetBuffering stdout NoBuffering
  putStr "> "
  input <- pack <$> getLine
  case parseSExpr input of
    Left err -> print err
    Right e -> do
      --   print e
      case eval e defaultEnv of
        Left err -> print err
        Right (result, env') -> do
          putStrLn $ unpack (stringOfSExpr result) ++ "\n"
          print env'
          repl env'
  repl env

main :: IO ()
main = do
  putStrLn "Welcome to the Raskell REPL!"
  repl defaultEnv
