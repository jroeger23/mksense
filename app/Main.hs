module Main where

import Data.List
import Control.Monad
import Mksense.Cmd
import Mksense.Parser.Logic as P
import Mksense.Parser.Cmd as C
import Mksense.Logic.Formula as F
import System.Environment

main :: IO ()
main = do
  a <- getLine
  let opts = C.run a in
    handle opts (P.run . rest $ opts)
