module Main where

import Control.Monad
import Mksense.Parser.Logic

main :: IO ()
main = forever $ do
  putStr "> "
  a <- getLine
  print $ run a
