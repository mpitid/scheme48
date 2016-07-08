
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
  deriving Show

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

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseNumber
         <|> parseString
         <|> parseAtom
         <|> parseQuoted
         <|> do
           char '('
           x <- try parseList <|> parseDottedList
           char ')'
           return x

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value " ++ show val

spaces :: Parser ()
spaces = skipMany1 space

main :: IO ()
main = do
  expr : _ <- getArgs
  putStrLn $ readExpr expr

