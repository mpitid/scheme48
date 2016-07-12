
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Control.Monad
import Control.Monad.Except
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import System.IO
import Data.IORef

data LispVal =
    Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func { params :: [String], vararg :: (Maybe String),
           body :: [LispVal], closure :: Env }

data LispError =
    NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

type Env = IORef [(String, IORef LispVal)]

-- A monad that may contain IO actions that throw a LispError
type IOThrowsError = ExceptT LispError IO

showError :: LispError -> String
showError (UnboundVar msg var) = msg ++ ": " ++ var
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NotFunction msg func) = msg ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

{- This is no longer necessary: http://stackoverflow.com/questions/31221410/adapting-error-to-except#31223291
instance Except LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default
-}

type ThrowsError = Either LispError -- curried type-constructor

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args) ++
    (case varargs of
      Nothing -> ""
      Just arg -> " . " ++ arg) ++ ") ...)"

instance Show LispVal where show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many parseStringContents
  char '"'
  return $ String x

parseStringContents :: Parser Char
parseStringContents =
  escapedChars <|> noneOf "\""

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  c <- oneOf "\"nrt\\"
  return $ case c of
    'n' -> '\n'
    't' -> '\t'
    '\\' -> '\\'
    _ -> c

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
-- parseNumber = do
--  digits <- many1 digit
--  return $ Number $ read digits
-- parseNumber = many1 digit >>= return . Number . read
{- Implement
   http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4
   but without exactness (just oct/hex really) -}
parseNumber = do
  prefixedNum <|> decimalDigits
  where prefixedNum = char '#' >> baseDigits
        decimalDigits = many1 digit >>= return . Number . read
        baseDigits = octal <|> hex <|> dec
        octal = char 'o' >> many1 octDigit >>= return . Number . fst . head . readOct
        hex = char 'x' >> many1 hexDigit >>= return . Number . fst . head . readHex
        dec = char 'd' >> decimalDigits
        -- binary = char 'b' >> oneOf '01' >> ??

parseLists :: Parser LispVal
parseLists = do
  head <- sepEndBy parseExpr spaces
  tail <- optionMaybe $ char '.' >> spaces >> parseExpr
  case tail of
    Nothing -> return $ List head
    Just tail -> return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseString
         <|> try parseNumber <|> parseAtom
         <|> parseQuoted
         <|> do
           char '('
           -- x <- try parseList <|> parseDottedList
           x <- parseLists
           char ')'
           return x

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

spaces :: Parser ()
spaces = skipMany1 space


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
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
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
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
  where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

--apply func args =
--  maybe (throwError $ NotFunction "Unrecognised primitive function" func) ($ args) $ lookup func primitives

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
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings =
  readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = newIORef value >>= return . (,) var
--          return $ env ++ map (\(k, v) -> (k, newIORef v)) bindings
-- foldl (\e (var, value) -> defineVar e var value) envRef bindings
--env <- readIORef envRef
-- return envRef

-- Working with multiple Monads is a pain

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

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


runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "lisp> ") . evalAndPrint

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    [expr] -> runOne expr
    otherwise -> putStrLn "usage: lisper [expr]"

