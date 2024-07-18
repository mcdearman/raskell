module Main (main) where

import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import Data.Void
import Debug.Trace
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Text.Pretty.Simple (pPrint, pShow)

-- defaultEnv :: Env
-- defaultEnv =
--   [ ( "+",
--       SNativeFn $ NativeFn $ \case
--         (Atom (AInt a) : xs) -> Right $ Atom $ AInt $ foldl (\acc (Atom (AInt x)) -> acc + x) a xs
--         [] -> Left $ RuntimeException "Must have at least one argument"
--         _ -> Left $ RuntimeException "Arguments must be integers"
--     ),
--     ( "-",
--       SNativeFn $ NativeFn $ \case
--         [Atom (AInt a), Atom (AInt b)] -> Right $ Atom $ AInt $ a - b
--         _ -> Left $ RuntimeException "Arguments must be integers"
--     ),
--     ( "*",
--       SNativeFn $ NativeFn $ \case
--         [Atom (AInt a), Atom (AInt b)] -> Right $ Atom $ AInt $ a * b
--         _ -> Left $ RuntimeException "Arguments must be integers"
--     ),
--     ( "/",
--       SNativeFn $ NativeFn $ \case
--         [Atom (AInt a), Atom (AInt b)] -> Right $ Atom $ AInt $ a `div` b
--         _ -> Left $ RuntimeException "Arguments must be integers"
--     ),
--     ( "%",
--       SNativeFn $ NativeFn $ \case
--         [Atom (AInt a), Atom (AInt b)] -> Right $ Atom $ AInt $ a `mod` b
--         _ -> Left $ RuntimeException "Arguments must be integers"
--     ),
--     ( "pow",
--       SNativeFn $ NativeFn $ \case
--         [Atom (AInt a), Atom (AInt b)] -> Right $ Atom $ AInt $ a ^ b
--         _ -> Left $ RuntimeException "Arguments must be integers"
--     ),
--     ( "=",
--       SNativeFn $ NativeFn $ \case
--         [a, b] -> Right $ Atom $ AKeyword $ if a == b then "t" else "f"
--         _ -> Left $ RuntimeException "must have two arguments"
--     ),
--     ( "neq",
--       SNativeFn $ NativeFn $ \case
--         [a, b] -> Right $ Atom $ AKeyword $ if a /= b then "t" else "f"
--         _ -> Left $ RuntimeException "must have two arguments"
--     ),
--     ( ">",
--       SNativeFn $ NativeFn $ \case
--         [Atom (AInt a), Atom (AInt b)] -> Right $ Atom $ AKeyword $ if a > b then "t" else "f"
--         _ -> Left $ RuntimeException "Arguments must be integers"
--     ),
--     ( ">=",
--       SNativeFn $ NativeFn $ \case
--         [Atom (AInt a), Atom (AInt b)] -> Right $ Atom $ AKeyword $ if a >= b then "t" else "f"
--         _ -> Left $ RuntimeException "Arguments must be integers"
--     ),
--     ( "<",
--       SNativeFn $ NativeFn $ \case
--         [Atom (AInt a), Atom (AInt b)] -> Right $ Atom $ AKeyword $ if a < b then "t" else "f"
--         _ -> Left $ RuntimeException "Arguments must be integers"
--     ),
--     ( "<=",
--       SNativeFn $ NativeFn $ \case
--         [Atom (AInt a), Atom (AInt b)] -> Right $ Atom $ AKeyword $ if a <= b then "t" else "f"
--         _ -> Left $ RuntimeException "Arguments must be integers"
--     ),
--     ( "not",
--       SNativeFn $ NativeFn $ \case
--         [Atom (AKeyword "t")] -> Right $ Atom $ AKeyword "f"
--         [Atom (AKeyword "f")] -> Right $ Atom $ AKeyword "t"
--         _ -> Left $ RuntimeException "Argument must be a boolean"
--     ),
--     ( "empty?",
--       SNativeFn $ NativeFn $ \case
--         [SList []] -> Right $ Atom $ AKeyword "t"
--         [SList _] -> Right $ Atom $ AKeyword "f"
--         _ -> Left $ RuntimeException "Argument must be a list"
--     ),
--     ( "pair",
--       SNativeFn $ NativeFn $ \case
--         [a, SList b] -> Right $ SList (a : b)
--         _ -> Left $ RuntimeException "Arguments must be a value and a list"
--     ),
--     ( "head",
--       SNativeFn $ NativeFn $ \case
--         [SList (x : _)] -> Right x
--         _ -> Left $ RuntimeException "Argument must be a non-empty list"
--     ),
--     ( "tail",
--       SNativeFn $ NativeFn $ \case
--         [SList (_ : xs)] -> Right $ SList xs
--         _ -> Left $ RuntimeException "Argument must be a non-empty list"
--     ),
--     ( "eval",
--       SNativeFn $ NativeFn $ \case
--         [x] -> eval x defaultEnv >>= \(result, _) -> Right result
--         _ -> Left $ RuntimeException "Must have exactly one argument"
--     )
--   ]

repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  putStr "> "
  input <- pack <$> getLine
  pPrint input
  repl

-- case parseSExpr input of
--   Left err -> pPrint err
--   Right e -> do
--     pPrint e
--     case eval e env of
--       Left err -> print err
--       Right (result, env') -> do
--         putStrLn $ unpack (textOfSExpr result) ++ "\n"
--         --   print env'
--         repl env'
-- repl env

main :: IO ()
main = do
  putStrLn "Welcome to the Raskell REPL!"
  repl
