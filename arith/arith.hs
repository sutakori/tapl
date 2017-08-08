module arith where

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          | Stuck
  deriving (Show,Eq)

isnumericval :: Term->Bool
isnumericval TmZero = True
isnumericval (TmSucc n) | isnumericval n = True
isnumericval _ = False

isval :: Term->Bool
isval TmTrue = True
isval TmFalse = True
isval nv | isnumericval nv = True
isval _ = False

isNoRuleApply :: Term -> Bool
isNoRuleApply n | isval n = True
                | (n == Stuck) = True
                | otherwise = False

eval1 :: Term->Term
eval1 (TmIf Stuck t1 t2) = Stuck
eval1 (TmIf TmTrue t1 t2) = t1
eval1 (TmIf TmFalse t1 t2) = t2
eval1 (TmIf t1 t2 t3) = TmIf t1' t2 t3
  where t1' = eval1 t1
eval1 (TmSucc Stuck) = Stuck
eval1 (TmSucc t1) = TmSucc t1'
  where t1' = eval1 t1
eval1 (TmPred Stuck) = Stuck
eval1 (TmPred TmZero) = TmZero
eval1 (TmPred (TmSucc nv1)) | isnumericval nv1 = nv1
eval1 (TmPred t1) = TmPred t1'
  where t1' = eval1 t1
eval1 (TmIsZero Stuck) = Stuck
eval1 (TmIsZero t1) = TmIsZero t1'
  where t1' = eval1 t1
eval1 (TmIsZero TmZero) = TmTrue
eval1 (TmIsZero nv1) | isnumericval nv1 = TmFalse
eval1 _ = Stuck

eval :: Term->Term
eval t | isNoRuleApply t = t
       | otherwise = eval (eval1 t)

