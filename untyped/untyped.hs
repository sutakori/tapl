module Untyped where

data Term = TmVar Integer
          | TmAbs Term
          | TmApply Term Term
  deriving(Show,Eq)

eval1 :: Term -> Term
eval1 (TmApply t1 t2) | (isValue t1) && (isValue t2) = termShift (-1) (termSubst 0 (termShift 1 t2) t1)
eval1 (TmApply t1 t2) | isValue t1 =TmApply t1 t2'
  where t2' = eval1 t2
eval1 (TmApply t1 t2) = TmApply t1' t2
  where t1' = eval1 t1

isValue :: Term -> Bool
isValue t@(TmAbs _) = True
isValue _ = False

termShift :: Integer -> Term -> Term
termShift d t = walk 0 t
  where walk c (TmVar k) | k<c = TmVar k
                         | otherwise = TmVar  (k+d)
        walk c (TmAbs t) = TmAbs (walk (c+1) t)
        walk c (TmApply t1 t2) = TmApply (walk c t1) (walk c t2)

--[j->s]t
termSubst :: Integer -> Term -> Term -> Term
termSubst j s t = innerExp (walk 0 t)
  where walk d (TmVar k) | k==j =termShift d s
                         | otherwise = (TmVar k)
        walk d (TmAbs t) = TmAbs (walk (d+1) t)
        walk d (TmApply t1 t2) = TmApply (walk d t1) (walk d t2)
        innerExp t@(TmAbs e) = e

eval :: Term -> Term
eval t | isValue t = t
eval t = eval t'
  where t' = eval1 t
