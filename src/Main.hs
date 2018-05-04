module Main where -- the entry point of the program lives here

import Syntax
import Parser
import Pretty
import Eval
import Data.Either
import System.Environment (getArgs)

main = do
  args <- getArgs
  let fname = if null args then error "no filename" else head args
  input <- readFile fname
  case parse program fname input of  
    Right v -> do
--      putStrLn $ pPrint v
      exec v >> return ()
    Left e -> print e
