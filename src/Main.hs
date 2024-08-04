module Main (main) where

import Data.Text (pack, unpack)
import Eval
import Reader
import SExpr
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Text.Pretty.Simple (pPrint)

-- repl :: Env -> IO ()
-- repl env = do
--   hSetBuffering stdout NoBuffering
--   putStr "> "
--   input <- pack <$> getLine
--   case readSExpr input of
--     Left err -> pPrint err
--     Right s -> do
--       pPrint s
--       case eval s env of
--         Left err -> print err
--         Right (result, env') -> do
--           putStrLn $ unpack (prettySExpr result) ++ "\n"
--           -- pPrint env'
--           repl env'
--   repl env
repl :: IO ()
repl = do
  hSetBuffering stdout NoBuffering
  putStr "> "
  input <- pack <$> getLine
  case readSExpr input of
    Left err -> pPrint err
    Right s -> do
      pPrint s
  repl

main :: IO ()
main = do
  putStrLn "Welcome to the Raskell REPL!"
  repl
