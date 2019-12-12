module Mksense.Cmd  where

import Mksense.Logic.Data
import qualified Mksense.Logic.Formula as F

data OutputScheme = Brief | Human
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
handle o = do if nnf o        then print . F.nnf
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
