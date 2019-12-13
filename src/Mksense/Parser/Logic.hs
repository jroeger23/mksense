module Mksense.Parser.Logic where

import Mksense.Parser.Core
import Mksense.Parser.Common
import Mksense.Logic.Data
import Data.Char
import Data.Maybe
import Control.Applicative

parens :: Parser Expression -> Parser Expression
parens m = do
  u <- unar
  spaces
  p <- oneOf $ map (reserved . fst) ps
  n <- m
  spaces
  reserved $ fromJust $ lookup p ps
  return $ n {unary=u}
  where ps = [("(",")"), ("[","]"), ("{","}"), ("<",">")]

unar :: Parser (Maybe Unary)
unar = do
  u <- oneOf [string "not", string "-", string "!"] <|> return []
  return $ (case u of []  -> Nothing ; _ -> Just Negate)

literal :: Parser Expression
literal = do
  u <- unar
  spaces
  l <- quoteStr <|> some alphanum
  spaces
  return $ Literal u (Right l)

expr :: Parser Expression
expr = oneOf [equivOp, impliesOp, orOp, andOp, xorOp, nandOp, atom]

atom :: Parser Expression
atom = parens expr <|> literal

infixOp :: [String] -> Operator -> Parser Expression
infixOp s o = do
  spaces
  a <- atom
  oneOf $ map reserved s
  b <- atom
  return $ Composed Nothing a o b

infixChain :: [String] -> Operator -> Parser Expression
infixChain s o = do
  spaces
  a <- atom
  oneOf $ map reserved s
  b <- infixChain s o <|> atom
  return $ Composed Nothing a o b


orOp :: Parser Expression
orOp = infixChain ["or", "||"] Or

andOp :: Parser Expression
andOp = infixChain ["and", "&&", "&"] And

impliesOp :: Parser Expression
impliesOp = infixOp ["=>", "->", "implies", "impl"] Implies

equivOp :: Parser Expression
equivOp = infixOp ["<=>", "=", "==", "<->", "equiv", "eq", "equivalent"] Equivalent

nandOp :: Parser Expression
nandOp = infixOp ["nand", "!&"] Nand

xorOp :: Parser Expression
xorOp = infixOp ["xor"] Xor


run :: String -> Expression
run = runParser expr
