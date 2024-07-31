module Eval where

import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import Debug.Trace (trace)
import RuntimeException
import SExpr
import Text.Pretty.Simple (pShow)

type Env = [(Text, SExpr)]

defaultEnv :: Env
defaultEnv =
  [ ( "+",
      SNativeFn $ NativeFn $ \case
        (SAtom (AInt a) : xs) ->
          let foldInts acc = \case
                (SAtom (AInt x) : xs') -> foldInts (acc + x) xs'
                [] -> Right $ SAtom $ AInt acc
                _ -> Left $ RuntimeException "Arguments must be integers"
           in foldInts a xs
        [] -> Left $ RuntimeException "Must have at least one argument"
        _ -> Left $ RuntimeException "Arguments must be integers"
    ),
    ( "-",
      SNativeFn $ NativeFn $ \case
        (SAtom (AInt a) : xs) ->
          let foldInts acc = \case
                (SAtom (AInt x) : xs') -> foldInts (acc - x) xs'
                [] -> Right $ SAtom $ AInt acc
                _ -> Left $ RuntimeException "Arguments must be integers"
           in foldInts a xs
        [] -> Left $ RuntimeException "Must have at least one argument"
        _ -> Left $ RuntimeException "Arguments must be integers"
    ),
    ( "*",
      SNativeFn $ NativeFn $ \case
        (SAtom (AInt a) : xs) ->
          let foldInts acc = \case
                (SAtom (AInt x) : xs') -> foldInts (acc * x) xs'
                [] -> Right $ SAtom $ AInt acc
                _ -> Left $ RuntimeException "Arguments must be integers"
           in foldInts a xs
        [] -> Left $ RuntimeException "Must have at least one argument"
        _ -> Left $ RuntimeException "Arguments must be integers"
    ),
    ( "/",
      SNativeFn $ NativeFn $ \case
        (SAtom (AInt a) : xs) ->
          let foldInts acc = \case
                SAtom (AInt 0) : _ -> Left $ RuntimeException "Division by zero"
                (SAtom (AInt x) : xs') -> foldInts (acc `div` x) xs'
                [] -> Right $ SAtom $ AInt acc
                _ -> Left $ RuntimeException "Arguments must be integers"
           in foldInts a xs
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
    ( "pair?",
      SNativeFn $ NativeFn $ \case
        [SList (_ : _)] -> Right $ SAtom $ AKeyword "t"
        [_] -> Right $ SAtom $ AKeyword "f"
        _ -> Left $ RuntimeException "Must have exactly one argument"
    ),
    ( "atom?",
      SNativeFn $ NativeFn $ \case
        [SAtom _] -> Right $ SAtom $ AKeyword "t"
        [SList []] -> Right $ SAtom $ AKeyword "t"
        [SList _] -> Right $ SAtom $ AKeyword "f"
        _ -> Left $ RuntimeException "Must have exactly one argument"
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
        _ -> Left $ RuntimeException "Arguments to ++ must be lists"
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
eval (SAtom (AInt x)) env = Right (SAtom $ AInt x, env)
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
  let expanded = expandQuasiquote x
   in trace ("expanded: " ++ unpack (toStrict (pShow expanded))) (Right (expanded, env))
  where
    expandQuasiquote :: SExpr -> SExpr
    expandQuasiquote (SList [SAtom (ASymbol "unquote"), y]) = y
    expandQuasiquote (SList [SAtom (ASymbol "unquote-splicing"), y]) =
      SList [SAtom (ASymbol "++"), y]
    expandQuasiquote (SList []) = SList [SAtom (ASymbol "quote"), SList []]
    expandQuasiquote (SList (x' : xs)) =
      SList
        [ SAtom (ASymbol "pair"),
          expandQuasiquote x',
          expandQuasiquote (SList xs)
        ]
    expandQuasiquote other = SList [SAtom (ASymbol "quote"), other]
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
    SLambda _ True _ -> do
      m <- trace "expansion: " (expandMacro sexpr fn_env)
      eval m env
    SNativeFn (NativeFn f) -> fmap (,fn_env) (f args')
    _ -> Left $ RuntimeException $ "First element of list must be a function got: " <> toStrict (pShow fn')
eval (SList []) _ = Left $ RuntimeException "Empty list"
eval e _ = Left $ RuntimeException ("Invalid expression: " <> pack (show e))