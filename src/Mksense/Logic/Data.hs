module Mksense.Logic.Data where


data Operator = And | Or | Implies | Equivalent | Nand | Xor deriving (Eq)
data Unary    = Negate deriving (Eq)
data Expression = Composed {
    unary :: Maybe Unary,
    lhs :: Expression,
    op :: Operator,
    rhs :: Expression
  } | Literal {
    unary :: Maybe Unary,
    value :: Either Bool String
  }

instance Eq Expression where
  (==) (Composed u a o b) (Composed u' a' o' b')
    | u == u' && o == o' && a == a' && b == b' = True
    | u == u' && o == o' && a == b' && b == a' = True
  (==) (Literal u a) (Literal u' a')
    | u == u' && a == a' = True
  (==) _ _ = False
instance Show Operator where
  show And        = "\x2227"
  show Or         = "\x2228"
  show Implies    = "\x21d2"
  show Equivalent = "\x21d4"
  show Nand       = "\x007c"
  show Xor        = "\x22bb"
instance Show Unary where
  show Negate = "\x00ac"
instance Show Expression where
  show = internal (cycle pars) (cycle colors)
    where
      pars   = [("( "," )") , ("[ "," ]") , ("{ "," }")]
      colors = ["\x1b[1;30;41m", "\x1b[1;30;42m", "\x1b[1;30;43m", "\x1b[1;30;44m", "\x1b[1;30;45m", "\x1b[1;30;46m", "\x1b[1;30;47m]"]
      internal (x:s) (x':s') (Composed Nothing a b c) = x' ++ (fst x) ++ internal s s' a ++ x' ++ " " ++ show b ++ " " ++ internal s s' c ++ x' ++ (snd x) ++ "\x1b[0m"
      internal s s' (Composed (Just u) a b c)         = show u ++ internal s s' (Composed Nothing a b c)
      internal _ _ (Literal Nothing (Right a))        = a
      internal _ _ (Literal (Just u) (Right a))       = show u ++ a
      internal _ _ (Literal (Just u) (Left a))        = show u ++ if a then "\x22a4" else "\x22a5"
      internal _ _ (Literal Nothing (Left a))         = if a then "\x22a4" else "\x22a5"
