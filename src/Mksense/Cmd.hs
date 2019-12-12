module Mksense.Cmd  where

import Mksense.Cmd as T
import Mksense.Logic.Data
import Mksense.Logic.Formula as F

data OutputScheme = Brief | Human deriving (Show)
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
  show o = (if T.nnf o then "nnf"
    else if T.dnf o    then "dnf"
    else if T.knf o    then "knf"
    else if T.orand o  then "orand"
    else "show")
    ++ if T.clauses o then " clauses" else ""


defaultOptions :: Options
defaultOptions = Options False False False False False Brief ""

handle :: Options -> Expression -> IO ()
handle o = do if T.nnf o        then print . F.nnf
              else if T.orand o then print . F.orand
              else if T.dnf o
                      then if T.clauses o
                              then print . F.minifyDnfClauses . F.dnfClauses . F.dnf . F.nnf . F.orand
                              else print . F.dnf . F.nnf . F.orand
              else if T.knf o
                      then if T.clauses o
                              then print . F.minifyKnfClauses . F.knfClauses . F.knf . F.nnf . F.orand
                              else print . F.knf . F.nnf . F.orand
              else print
