module Mksense.Cmd where

import Mksense.Logic.Data

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

handle :: Options -> Expression -> Expression
handle _ = id
