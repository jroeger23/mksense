module Mksense.Parser.Common where

import Mksense.Parser.Core
import Data.Char
import Control.Applicative

char :: Char -> Parser Char
char c = satisfy (c ==)

nchar :: Char -> Parser Char
nchar c = satisfy (c /=)

string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs)}

token :: Parser a -> Parser a
token p = do { a <- p; spaces ; return a}

reserved :: String -> Parser String
reserved s = token (string s)

spaces :: Parser String
spaces = many . satisfy $ flip elem " \n\r"

alphanum :: Parser Char
alphanum = satisfy isAlphaNum

quoteStr :: Parser String
quoteStr = do
  q <- oneOf $ map char qs
  s <- many $ nchar q
  char q
  return s
  where qs = ['\"', '\'']
