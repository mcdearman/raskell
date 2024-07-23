module Main (main) where

import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import Data.Void
import Debug.Trace
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
import Text.Pretty.Simple (pPrint, pShow)

data SExpr
  = SAtom Atom
  | SList [SExpr]
  | Cons SExpr SExpr
  | SLambda [Text] Bool SExpr
  | SNativeFn NativeFn
  deriving (Eq, Show)

textOfSExpr :: SExpr -> Text
textOfSExpr (SAtom (AInt x)) = pack $ show x
textOfSExpr (SAtom (AReal x)) = pack $ show x
textOfSExpr (SAtom (AString x)) = pack $ show x
textOfSExpr (SAtom (ASymbol x)) = x
textOfSExpr (SAtom (AKeyword x)) = ":" <> x
textOfSExpr (SList xs) = "(" <> pack (unwords $ map (unpack . textOfSExpr) xs) <> ")"
textOfSExpr (Cons x y) = "(" <> textOfSExpr x <> " . " <> textOfSExpr y <> ")"
textOfSExpr (SLambda {}) = "<lambda>"
textOfSExpr (SNativeFn _) = "<nativeFn>"

newtype NativeFn = NativeFn ([SExpr] -> Either RuntimeException SExpr)

instance Eq NativeFn where
  _ == _ = False

instance Show NativeFn where
  show _ = "<nativeFn>"

data Atom
  = AInt Integer
  | AReal Double
  | AString String
  | ASymbol Text
  | AKeyword Text
  deriving (Eq, Show)

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

-- Symbols can be any sequence of characters that are not whitespace, parens, or quotes
symbolParser :: Parser Text
symbolParser =
  lexeme $ takeWhile1P (Just "symbol") (\c -> c `notElem` [' ', '\n', '\r', '\t', '(', ')', '\'', '`', ','])

keyword :: Parser Atom
keyword = lexeme (char ':' *> (AKeyword <$> symbolParser))

atom :: Parser Atom
atom =
  choice
    [ AInt <$> int,
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
list = SList <$> parens (many sexpr)

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
        (SAtom (AInt a) : xs) -> Right $ SAtom $ AInt $ foldl (\acc (SAtom (AInt x)) -> acc + x) a xs
        [] -> Left $ RuntimeException "Must have at least one argument"
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
    ),
    ( "%",
      SNativeFn $ NativeFn $ \case
        [SAtom (AInt a), SAtom (AInt b)] -> Right $ SAtom $ AInt $ a `mod` b
        _ -> Left $ RuntimeException "Arguments must be integers"
    ),
    ( "pow",
      SNativeFn $ NativeFn $ \case
        [SAtom (AInt a), SAtom (AInt b)] -> Right $ SAtom $ AInt $ a ^ b
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
        [SAtom (AInt a), SAtom (AInt b)] -> Right $ SAtom $ AKeyword $ if a > b then "t" else "f"
        _ -> Left $ RuntimeException "Arguments must be integers"
    ),
    ( ">=",
      SNativeFn $ NativeFn $ \case
        [SAtom (AInt a), SAtom (AInt b)] -> Right $ SAtom $ AKeyword $ if a >= b then "t" else "f"
        _ -> Left $ RuntimeException "Arguments must be integers"
    ),
    ( "<",
      SNativeFn $ NativeFn $ \case
        [SAtom (AInt a), SAtom (AInt b)] -> Right $ SAtom $ AKeyword $ if a < b then "t" else "f"
        _ -> Left $ RuntimeException "Arguments must be integers"
    ),
    ( "<=",
      SNativeFn $ NativeFn $ \case
        [SAtom (AInt a), SAtom (AInt b)] -> Right $ SAtom $ AKeyword $ if a <= b then "t" else "f"
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
    let newEnv = zip params args ++ env
    eval body newEnv >>= \(result, _) -> Right result
  _ -> Left $ RuntimeException "Not a macro"
expandMacro _ _ = Left $ RuntimeException "Invalid macro call"

eval :: SExpr -> Env -> Either RuntimeException (SExpr, Env)
eval (SAtom (AInt x)) env = Right (SAtom $ AInt x, env)
eval (SAtom (AReal x)) env = Right (SAtom $ AReal x, env)
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
      -- pPrint e
      case eval e env of
        Left err -> print err
        Right (result, env') -> do
          putStrLn $ unpack (textOfSExpr result) ++ "\n"
          -- pPrint env'
          repl env'
  repl env

main :: IO ()
main = do
  putStrLn "Welcome to the Raskell REPL!"
  repl defaultEnv
