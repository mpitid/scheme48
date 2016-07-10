
module Main where

import Control.Monad
import Control.Monad.Error
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

data LispVal =
    Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool

data LispError =
    NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String


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

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

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


eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Atom _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe (throwError $ NotFunction "Unrecognised primitive function" func) ($ args) $ lookup func primitives

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
  ]

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

main :: IO ()
main = do
  args <- getArgs
  -- >>= has higher precendence than $, so this reads like this:
  -- liftM show (readExpr (args !! 0) >>= eval)
  -- We need the return to enter the IO monad, and then use <- to get out (well bind to the next action really).
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled

