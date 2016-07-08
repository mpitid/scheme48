
module Main where

import Control.Monad
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

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

spaces :: Parser ()
spaces = skipMany1 space


eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [
    ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) =
  let parsed = reads n :: [(Integer, String)] in
  if null parsed then 0 else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

