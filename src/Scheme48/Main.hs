
module Scheme48.Main (
    scheme48
  ) where

import System.IO
import System.Environment
import Control.Monad.Except

import Scheme48.Core
import Scheme48.Parser
import Scheme48.Environment
import Scheme48.Interpreter

-- Execute a sequence of statements or bring up an interactive REPL.
scheme48 :: IO ()
scheme48 = do
  args <- getArgs
  if null args then runRepl else runOne args

-- IO helpers

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
-- >>= has higher precendence than $, so this reads like this:
-- liftM show (readExpr (args !! 0) >>= eval)
evalString env expr =
  runIOThrows (liftM show $ (liftThrows $ readExpr expr) >>= eval env)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- The underscore is convention for monadic functions that repeat without returning a value.
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action


--runOne :: String -> IO ()
--runOne expr = primitiveBindings >>= flip evalAndPrint expr
-- Load statements from first argument and bind the rest to an `args` variable
runOne :: [String] -> IO ()
runOne (file : args) = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String file])) >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "lisp> ") . evalAndPrint

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue


