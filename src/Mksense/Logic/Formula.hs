module Mksense.Logic.Formula where

import Mksense.Logic.Data
import Data.List
import Data.Type.Equality


-- Logic types
type Clause = [Expression]
type KClause = [Expression]
type DClause = [Expression]

 -- Check if two clauses are equivalent
(<=>) :: Clause -> Clause -> Bool
(<=>) a b = all (\x -> x `elem` a) b && all (\x -> x `elem` b) a

-- negate an expression
neg :: Expression -> Expression
neg (Literal (Just Negate) a)      = (Literal Nothing a)
neg (Literal Nothing a)            = (Literal (Just Negate) a)
neg (Composed (Just Negate) a o b) = (Composed Nothing a o b)
neg (Composed Nothing a o b)       = (Composed (Just Negate) a o b)

-- create nnf of expression
nnf :: Expression -> Expression
nnf (Composed (Just Negate) a And b)          = Composed Nothing (nnf . neg $ a) Or (nnf . neg $ b)
nnf (Composed (Just Negate) a Or b)           = Composed Nothing (nnf . neg $ a) And (nnf . neg $ b)
nnf (Composed (Just Negate) a Implies b)      = Composed Nothing (nnf a) And (nnf . neg $ b)
nnf (Composed (Just Negate) a Equivalent b)   = Composed Nothing (nnf a) Xor (nnf b)
nnf (Composed (Just Negate) a Nand b)         = Composed Nothing (nnf a) And (nnf b)
nnf (Composed (Just Negate) a Xor b)          = Composed Nothing (nnf a) Equivalent (nnf b)
nnf (Composed Nothing       a o b)            = Composed Nothing (nnf a) o (nnf b)
nnf (Literal u (Left a)) | u == (Just Negate) = (Literal Nothing (Left (not a)))
nnf a                                         = a

-- create expression with only disjunctions and conjunctions of another expression
orand :: Expression -> Expression
orand (Composed u a Implies b)    = Composed u (orand . neg $ a) Or (orand b)
orand (Composed u a Equivalent b) = Composed u (Composed Nothing (orand a) And (orand b)) Or (Composed Nothing (orand . neg $ a) And (orand . neg $ b))
orand (Composed u a Nand b)       = Composed u (orand . neg $ a) Or (orand . neg $ b)
orand (Composed u a Xor b)        = Composed u (Composed Nothing (orand . neg $ a) And (orand b)) Or (Composed Nothing (orand a) And (orand . neg $ b))
orand (Composed u a o b)          = Composed u (orand a) o (orand b)
orand (Literal u a)               = Literal u a

-- create conjunction normal form of expression
cnf :: Expression -> Expression
cnf e
  | iscnf e   = e
  | otherwise = cnf . rearrange $ e
  where
    rearrange (Literal u a) = Literal u a
    rearrange (Composed u a Or (Composed u' a' And b')) = Composed u (Composed Nothing (rearrange a) Or (rearrange a')) And (Composed Nothing (rearrange a) Or (rearrange b'))
    rearrange (Composed u (Composed u' a' And b') Or b) = Composed u (Composed Nothing (rearrange b) Or (rearrange a')) And (Composed Nothing (rearrange b) Or (rearrange b'))
    rearrange (Composed u a o b)                        = Composed u (rearrange a) o (rearrange b)

-- create disjunction normal form of expression
dnf :: Expression -> Expression
dnf e
  | isdnf e   = e
  | otherwise = dnf . rearrange $ e
  where
    rearrange (Literal u a) = Literal u a
    rearrange (Composed u a And (Composed u' a' Or b')) = Composed u (Composed Nothing (rearrange a) And (rearrange a')) Or (Composed Nothing (rearrange a) And (rearrange b'))
    rearrange (Composed u (Composed u' a' Or b') And b) = Composed u (Composed Nothing (rearrange b) And (rearrange a')) Or (Composed Nothing (rearrange b) And (rearrange b'))
    rearrange (Composed u a o b)                        = Composed u (rearrange a) o (rearrange b)

-- extract clauses of cnf
cnfClauses :: Expression -> [KClause]
cnfClauses (Composed Nothing a o b)
  | o == And = cnfClauses a ++ cnfClauses b
  | o == Or  = [clause a ++ clause b]
  where
    clause (Composed u a Or b) = clause a ++ clause b
    clause (Literal u a)       = [(Literal u a)]
cnfClauses (Literal u a) = [[(Literal u a)]]

-- extract clauses of dnf
dnfClauses :: Expression -> [DClause]
dnfClauses (Composed Nothing a o b)
  | o == Or = dnfClauses a ++ dnfClauses b
  | o == And  = [clause a ++ clause b]
 where
    clause (Composed u a And b) = clause a ++ clause b
    clause (Literal u a)       = [(Literal u a)]
dnfClauses (Literal u a) = [[(Literal u a)]]

-- minify each clause
minifyCnfClauses :: [KClause] -> [KClause]
minifyCnfClauses = rmdups [] . minify
  where
    minify (c:cs) = let mc = minifyCnfClause c in
      if mc == [(Literal Nothing (Left False))]
      then minify cs else mc:minify cs
    minify [] = []
    rmdups seen [] = seen
    rmdups seen (a:s)
      | any (a <=>) seen = rmdups seen s
      | otherwise        = rmdups (a:seen) s

-- remove duplicates and check if universally valid
minifyCnfClause :: KClause -> KClause
minifyCnfClause = internal []
  where
    internal seen [] = seen
    internal seen ((Literal u a):s)
      | (neg $ Literal u a) `elem` seen = [(Literal Nothing (Left True))]
      | (Literal u a) `elem` seen       = internal seen s
      | otherwise                       = internal ((Literal u a):seen) s

-- minify each clause
minifyDnfClauses :: [DClause] -> [DClause]
minifyDnfClauses = rmdups [] . minify
  where
    minify (c:cs) = let mc = minifyDnfClause c in
      if mc == [(Literal Nothing (Left False))]
      then minify cs else mc:minify cs
    minify [] = []
    rmdups seen [] = seen
    rmdups seen (a:s)
      | any (a <=>) seen = rmdups seen s
      | otherwise        = rmdups (a:seen) s

-- remove duplicates and check if unacomplishable
minifyDnfClause :: DClause -> DClause
minifyDnfClause = internal []
  where
    internal seen [] = seen
    internal seen ((Literal u a):s)
      | (neg $ Literal u a) `elem` seen = [(Literal Nothing (Left False))]
      | (Literal u a) `elem` seen       = internal seen s
      | otherwise                       = internal ((Literal u a):seen) s

-- Verification functions
isDclause :: Expression -> Bool
isDclause (Composed Nothing a Or b) = (isDclause a) && (isDclause b)
isDclause (Literal _ _)             = True
isDclause _                         = False

isCclause :: Expression -> Bool
isCclause (Composed Nothing a And b) = (isCclause a) && (isCclause b)
isCclause (Literal _ _)              = True
isCclause _                          = False

iscnf :: Expression -> Bool
iscnf (Composed Nothing a Or b)                            = (isDclause a) && (isDclause b)
iscnf (Composed Nothing a And (Composed Nothing a' Or b')) = (iscnf a) && (isDclause a') && (isDclause b')
iscnf (Composed Nothing (Composed Nothing a' Or b') And b) = (iscnf b) && (isDclause a') && (isDclause b')
iscnf (Composed Nothing a And b)                           = (iscnf a) && (iscnf b)
iscnf (Literal _ _)                                        = True
iscnf _                                                    = False

isdnf :: Expression -> Bool
isdnf (Composed Nothing a And b)                           = (isCclause a) && (isCclause b)
isdnf (Composed Nothing a Or (Composed Nothing a' And b')) = (isdnf a) && (isCclause a') && (isCclause b')
isdnf (Composed Nothing (Composed Nothing a' And b') Or b) = (isdnf b) && (isCclause a') && (isCclause b')
isdnf (Composed Nothing a Or b)                            = (isdnf a) && (isdnf b)
isdnf (Literal _ _)                                        = True
isdnf _                                                    = False
