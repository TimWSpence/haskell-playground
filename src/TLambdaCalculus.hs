{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TLambdaCalculus where

-- We use De Bruijn indices for variables to avoid
-- various kinds of alpha-equivalence pain
newtype Idx = Idx { getIdx :: Int } deriving (Eq, Ord, Num, Show)

data Exp = EVar Idx
         | ELambda Exp
         | EApp Exp Exp
         deriving Show

relabel :: Idx -> Idx -> Exp -> Exp
relabel c d (EVar n) = if (n >= c) then EVar (n + d) else (EVar n)
relabel c d (ELambda e) = ELambda $ relabel (c+1) d e
relabel c d (EApp e1 e2) = EApp (relabel c d e1) (relabel c d e2)

subst :: Idx -> Exp -> Exp -> Exp
subst n e e1@(EVar m) = if (n == m) then e else e1
subst n e (ELambda e1) = relabel 0 (-1) $ subst n (relabel 0 1 e) e1
subst n e (EApp e1 e2) = EApp (subst n e e1) (subst n e e2)

betaReduce :: Exp -> Exp
betaReduce e@(EVar _) = e
betaReduce e@(ELambda e1) = e
betaReduce (EApp e1 e2) = betaReduce $ subst 0 e1 (betaReduce e2)