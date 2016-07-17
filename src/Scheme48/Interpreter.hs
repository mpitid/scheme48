
{-# LANGUAGE ExistentialQuantification #-}

module Scheme48.Interpreter (
    eval
  , primitiveBindings
  ) where

import System.IO
import Control.Monad.Except
import Text.ParserCombinators.Parsec (Parser, parse)

import Scheme48.Core
import Scheme48.Parser (readExpr, readExprList)
import Scheme48.Environment


data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    Bool False -> eval env alt
    Bool True  -> eval env conseq
    otherwise  -> throwError $ TypeMismatch "bool" otherwise
    --otherwise  -> eval conseq
--eval env  (List ((Atom "cond") : expressions)) = cond expressions
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body
eval env (List [Atom "load", String filename]) =
  load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
    then throwError $ NumArgs (num params) args
    else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
          Nothing -> return env

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                               ++ map (makeFunc PrimitiveFunc) primitives)
  where makeFunc constructor (var, func) = (var, constructor func)

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

--apply func args =
--  maybe (throwError $ NotFunction "Unrecognised primitive function" func) ($ args) $ lookup func primitives

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [
    ("apply", applyProc)
  , ("open-input-file", makePort ReadMode)
  , ("open-output-file", makePort WriteMode)
  , ("close-input-port", closePort)
  , ("close-output-port", closePort)
  , ("read", readProc)
  , ("write", writeProc)
  , ("read-contents", readContents)
  , ("read-all", readAll)
  ]

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
    ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  , ("boolean?", booleanUnaryOp isBoolean)
  , ("symbol?", booleanUnaryOp isSymbol)
  , ("number?", booleanUnaryOp isNumber)
  , ("string?", booleanUnaryOp isString)
  , ("symbol->string", symbolToString)
  , ("=", numBoolBinop (==))
  , ("<", numBoolBinop (<))
  , (">", numBoolBinop (>))
  , ("/=", numBoolBinop (/=))
  , ("<=", numBoolBinop (<=))
  , (">=", numBoolBinop (>=))
  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))
  , ("string=?", strBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("eq?", eqv)
  , ("eqv?", eqv)
  , ("equal?", equal)
  ]

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op [a1, a2] = do
  left <- unpacker a1
  right <- unpacker a2
  return $ Bool $ left `op` right
boolBinop _ _ args = throwError $ NumArgs 2 args

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op param@[_]  = throwError $ NumArgs 2 param
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n in
  if null parsed
  then throwError $ TypeMismatch "number" $ String n
  else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

booleanUnaryOp :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
booleanUnaryOp op [arg] = return $ Bool $ op arg
booleanUnaryOp op args = throwError $ NumArgs 2 args

isSymbol :: LispVal -> Bool
isSymbol (Atom _) = True
isSymbol _ = False

isBoolean :: LispVal -> Bool
isBoolean (Bool _) = True
isBoolean _ = False

isString :: LispVal -> Bool
isString (String _) = True
isString _ = False

isNumber :: LispVal -> Bool
isNumber (Number _) = True
isNumber _ = False

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [Atom s] = Right $ String s
symbolToString [term] = Left $ TypeMismatch "atom" term
symbolToString terms =  Left $ NumArgs 1 terms

-- List-manipulation primitives:

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)]         = return $ List xs
cdr [DottedList [_] t]      = return t
cdr [DottedList (_ : xs) t] = return $ DottedList xs t
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs]         = return $ List $ x : xs
cons [x, DottedList xs t] = return $ DottedList (x : xs) t
cons [x, t]               = return $ DottedList [x] t
cons badArgList           = throwError $ NumArgs 2 badArgList

-- Equivalence
eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2]             = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2]         = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2]         = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2]             = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [a@(List xs), b@(List ys)] = listEquals (\x y -> eqv [x, y]) a b
eqv [_, _]                             = return $ Bool False
eqv badArgList                         = throwError $ NumArgs 2 badArgList


unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
    un1 <- unpacker arg1
    un2 <- unpacker arg2
    return $ un1 == un2
  -- I like this curried const trick, used because catchError expects a function
  `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [x@(List _), y@(List _)] = listEquals (\a b -> equal [a, b]) x y
equal [DottedList xs x, DottedList ys y] = listEquals (\a b -> equal [a, b]) (List $ xs ++ [x]) (List $ ys ++ [y])
equal [x, y]     = do
  primitiveEquals <- liftM or $ mapM (unpackEquals x y)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  --eqvEquals <- eqv [x, y]
  --return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
  Bool x' <- eqv [x, y]
  return $ Bool $ (primitiveEquals || x')
equal badArgList = throwError $ NumArgs 2 badArgList

listEquals :: (LispVal -> LispVal -> ThrowsError LispVal)
           -> LispVal -> LispVal -> ThrowsError LispVal
listEquals cmp (List xs) (List ys) =
    return $ Bool $ (length xs == length ys)  && (all eqvPair $ zip xs ys)
  where eqvPair (x, y) = case cmp x y of
                           Left err -> False
                           Right (Bool val) -> val

{-
-- TODO: Ensure `else` can only appear in the last place, would require manual recursion
-- TODO: NumArgs with range of arguments, e.g. > 1
cond :: Env -> [LispVal] -> ThrowsError LispVal
cond env []      = throwError $ NumArgs 1 []
cond env clauses = do
  -- if result is [Nothing Nothing (Just x) ...], we want (Just x)
  results <- mapM (clause env) clauses
  case foldl1 (\r c -> case r of Nothing -> c; Just x -> Just x) results of
    Nothing -> throwError $ BadSpecialForm "cond with no matching clause near" (head clauses)
    Just x -> return x
--cond clauses = (mapM evalClause clauses) >>= return . last
-- cond clauses = foldl (\p e -> case p of Left erreval e) (return $ Bool True) clauses

clause :: Env -> LispVal -> IOThrowsError (Maybe LispVal)
clause env (List ((Atom "else") : exprs)) = clause env (List (Bool True : exprs))
clause env (List x@[test, Atom "=>", expr]) = throwError $ BadSpecialForm "not implemented yet" (Atom "=>")
clause env (List (test : exprs)) = do
  testResult <- eval env test
  case testResult of
    Bool True -> clauseExpr exprs >>= return . Just
    Bool False -> return Nothing
    otherwise -> throwError $ TypeMismatch "bool" otherwise

clauseExpr :: Env -> [LispVal] -> IOThrowsError LispVal
clauseExpr env exprs = mapM (eval env) exprs >>= return . last
-}

