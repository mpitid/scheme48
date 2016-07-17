
module Scheme48.Environment where

import Scheme48.Core

import Data.IORef
import Control.Monad
import Control.Monad.Except

-- Environment

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

-- Only a single monad can be referenced in a do block
-- so we need to lift the ref from the IO monad to the combined IOError monad
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
        -- flip changes argument order to create our write closure
        (liftIO . (flip writeIORef value))
        (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
     then setVar envRef var value
     else liftIO $ do
          valueRef <- newIORef value
          env <- readIORef envRef
          writeIORef envRef ((var, valueRef) : env)
          return value

-- Why do we need to create a new IORef at the end of this given that it's stateful?
-- Because of lexical scoping!
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings =
  readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = newIORef value >>= return . (,) var
--          return $ env ++ map (\(k, v) -> (k, newIORef v)) bindings
-- foldl (\e (var, value) -> defineVar e var value) envRef bindings
--env <- readIORef envRef
-- return envRef

