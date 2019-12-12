module Main where

import Control.Monad
import Mksense.Cmd
import Mksense.Parser.Logic as L
import Mksense.Parser.Cmd as C
import System.Environment
import System.IO


interactive :: Options -> IO ()
interactive opts = forever $ do
  putStr $ show opts ++ "> "
  hFlush stdout
  a <- getLine
  handle opts (L.run a)

main :: IO ()
main = do
  a <- getArgs
  let opts = C.run $ foldr (\a b -> a++" "++b) "" a in
    if rest opts == "" then
      interactive opts
    else
      handle opts (L.run . rest $ opts)
