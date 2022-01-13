module Main where

import Eval (eval)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  input <- getLine
  case input of
    "exit" -> return ()
    "quit" -> return ()
    _ -> do
      putStrLn $ eval input
      main
