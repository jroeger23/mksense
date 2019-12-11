module Mksense.Cmd where

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
  } deriving (Show)

defaultOptions :: Options
defaultOptions = Options False False False False False Brief ""

handle :: Options -> Expression -> IO ()
handle o = do if Mksense.Cmd.nnf o        then print . F.nnf
              else if Mksense.Cmd.orand o then print . F.orand
              else if Mksense.Cmd.dnf o
                      then if Mksense.Cmd.clauses o
                              then print . F.minifyDnfClauses . F.dnfClauses . F.dnf . F.nnf . F.orand
                              else print . F.dnf . F.nnf . F.orand
              else if Mksense.Cmd.knf o
                      then if Mksense.Cmd.clauses o
                              then print . F.minifyKnfClauses . F.knfClauses . F.knf . F.nnf . F.orand
                              else print . F.knf . F.nnf . F.orand
              else print
