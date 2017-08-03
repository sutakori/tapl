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

eval :: Term -> Term
eval t | isval t = t
eval (TmIf t1 t2 t3) | eval t1 == Stuck = Stuck
                     | eval t1 == TmTrue = v2
                     | eval t1 == TmFalse = v3
  where v2 = eval t2
        v3 = eval t3
eval (TmPred TmZero) = TmZero
eval (TmPred (TmSucc t)) | isnumericval nv = nv
                         | otherwise = Stuck
  where nv = eval t
eval (TmIsZero t) | v == TmZero = TmTrue
                  | otherwise = TmFalse
  where v = eval t
eval _ = Stuck
