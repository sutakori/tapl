data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term

isnumericval :: Term->Bool
isnumericval TmZero = True
isnumericval (TmSucc n) | isnumericval n = True
isnumericval _ = False

isval :: Term->Bool
isval TmTrue = True
isval TmFalse = True
isval nv | isnumericval nv = True
isval _ = False


eval1 :: Term->Maybe Term
eval1 (TmIf TmTrue t1 t2) = Just t1
eval1 (TmIf TmFalse t1 t2) = Just t2
eval1 (TmIf t1 t2 t3) = Just (TmIf t1' t2 t3)
  where t1' = eval1 t1
eval1 (TmSucc t1) = Just (TmSucc t1')
  where t1' = eval1 t1
eval1 (TmPred t1) = Just (TmPred t1')
  where t1' = eval1 t1
eval1 (TmPred TmZero) = Just TmZero
eval1 (TmPred (TmSucc nv1)) | isnumericval nv1 = Just nv1
eval1 (TmIsZero t1) = Just (TmIsZero t1')
  where t1' = eval1 t1
eval1 (TmIsZero TmZero) = Just TmTrue
eval1 (TmIsZero nv1) | isnumericval nv1 = Just TmFalse
eval1 _ = Nothing

eval :: Term->Maybe Term
eval t = eval t'
  where t' = eval1 t

