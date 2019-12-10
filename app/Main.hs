module Main where

import Data.List
import Control.Monad
import Mksense.Parser.Logic as P
import Mksense.Logic.Formula as F
import System.Environment

main :: IO ()
main = do
  --a <- getLine
  args <- getArgs
  case args of
    "nnf":expr   -> print $ nnf $ P.run $ intercalate " " expr
    "dnf":expr   -> print $ dnf . nnf . orand $ P.run $ intercalate " " expr
    "knf":expr   -> print $ knf . nnf . orand $ P.run $ intercalate " " expr
    "orand":expr -> print $ orand $ P.run $ intercalate " " expr
    "knfcl":expr -> print $ minifyKnfClauses . knfClauses . knf . nnf . orand $ P.run $ intercalate " " expr
    "dnfcl":expr -> print $ minifyDnfClauses . dnfClauses . dnf . nnf . orand $ P.run $ intercalate " " expr
    expr         -> print $ P.run $ intercalate " " expr
