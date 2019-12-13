module Main where

import Control.Monad
import Mksense.Cmd
import Mksense.Parser.Logic as L
import Mksense.Parser.Cmd as C
import System.Environment
import System.IO


getUntilLFLF :: Char -> IO String
getUntilLFLF prev = do
  a <- getChar
  if a == '\n' && prev == a then do
    return ""
  else do
    b <- getUntilLFLF a
    return $ a:b


interactive :: Options -> IO ()
interactive opts = forever $ do
  putStr $ "\n" ++ show opts ++ "> "
  hFlush stdout
  a <- getUntilLFLF ' '
  putStrLn "" >> handle opts (L.run a)

main :: IO ()
main = do
  a <- getArgs
  let opts = C.run $ foldr (\a b -> a++" "++b) "" a in
    if rest opts == "" then
      interactive opts
    else
      handle opts (L.run . rest $ opts)
