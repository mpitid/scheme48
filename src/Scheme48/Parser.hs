
module Scheme48.Parser (
    readExpr
  , readExprList
  ) where

import Numeric
import Control.Monad.Except (throwError)
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Char (endOfLine)
import Scheme48.Core

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow parseExpressions

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val


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

parseExpressions :: Parser [LispVal]
parseExpressions =
  optional whiteSpace >> endBy parseExpr whiteSpace

spaces :: Parser ()
spaces = skipMany1 space

comment :: Parser String
comment =
  char ';' >> manyTill anyChar endOfLine

whiteSpace :: Parser ()
whiteSpace =
  skipMany $ (skip comment) <|> (skip space)

skip :: Parser a -> Parser ()
skip p =
  p >> return ()
