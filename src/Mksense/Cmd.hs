module Mksense.Cmd  where

import Mksense.Logic.Data
import qualified Mksense.Logic.Formula as F

data OutputScheme = Brief | Human deriving (Eq)
data Options = Options
  {
    nnf :: Bool,
    dnf :: Bool,
    knf :: Bool,
    orand :: Bool,
    clauses :: Bool,
    scheme :: OutputScheme,
    rest :: String
  }

instance Show Options where
  show o = (if nnf o then "nnf"
    else if dnf o    then "dnf"
    else if knf o    then "knf"
    else if orand o  then "orand"
    else "show")
    ++ if clauses o then " clauses" else ""


defaultOptions :: Options
defaultOptions = Options False False False False False Brief ""

handle :: Options -> Expression -> IO ()
handle o = do
  if scheme o == Human then human o
    else
              if nnf o        then print . F.nnf
              else if orand o then print . F.orand
              else if dnf o
                      then if clauses o
                              then print . F.minifyDnfClauses . F.dnfClauses . F.dnf . F.nnf . F.orand
                              else print . F.dnf . F.nnf . F.orand
              else if knf o
                      then if clauses o
                              then print . F.minifyKnfClauses . F.knfClauses . F.knf . F.nnf . F.orand
                              else print . F.knf . F.nnf . F.orand
              else print

human :: Options -> Expression -> IO ()
human o e
  | clauses o = do
      putStr "Expression: "
      print e
      if dnf o then
        let clauses = F.minifyDnfClauses . F.dnfClauses . F.dnf . F.nnf . F.orand $ e in
          if clauses == [] then putStrLn "Is never Satisfied!"
          else
          foldl (>>) (putStrLn "Is Satisfield when either:")
            $ map (\cs -> putStrLn $ " - Each of: "++show cs++" evaluates to True") clauses
      else if knf o then
        let clauses = F.minifyKnfClauses . F.knfClauses . F.knf . F.nnf . F.orand $ e in
          foldl (>>) (putStrLn "Is Satisfield when all of:")
            $ map (\cs -> putStrLn $ " - One of: "++show cs++" evaluates to True") clauses
      else error "Cannot show clauses in show mode"
  | dnf o = do
      putStr "Expression              : "
      print e
      putStr "Has Disjunct Normal Form: "
      print . F.dnf . F.nnf . F.orand $ e
  | knf o = do
      putStr "Expression              : "
      print e
      putStr "Has Conjunct Normal Form: "
      print . F.knf . F.nnf . F.orand $ e
  | orand o = do
      putStr "Expression                 : "
      print e
      putStr "Displayed only with or, and: "
      print . F.orand $ e
  | nnf o = do
      putStr "Expression              : "
      print e
      putStr "Has Negation Normal Form: "
      print . F.nnf $ e
  | otherwise = do print e
