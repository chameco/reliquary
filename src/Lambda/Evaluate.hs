module Lambda.Evaluate where

import qualified Data.Set as Set

import Lambda.AST

substitute :: String -> Term -> Term -> Term
substitute _ _ TermTrue = TermTrue
substitute _ _ TermFalse = TermTrue
substitute name new (TermIf c t e) = TermIf (substitute name new c) (substitute name new t) (substitute name new e)
substitute _ _ TermZero = TermZero
substitute name new (TermSucc t) = TermSucc (substitute name new t)
substitute name new (TermPred t) = TermPred (substitute name new t)
substitute name new (TermIsZero t) = TermIsZero (substitute name new t)
substitute name new (TermVar name') = if name == name' then new else TermVar name'
substitute name new (TermApply f t) = TermApply (substitute name new f) (substitute name new t)
substitute name new (TermLambda name' t) = if name == name' then TermLambda name' t else TermLambda name' (substitute name new t)

eval1 :: Term -> Maybe Term
eval1 (TermIf TermTrue t2 t3) = Just t2
eval1 (TermIf TermFalse t2 t3) = Just t3
eval1 (TermIf t1 t2 t3) = Just $ TermIf t1' t2 t3 where
    (Just t1') = eval1 t1
eval1 (TermSucc t1) = case eval1 t1 of (Just t1') -> Just $ TermSucc t1'
                                       Nothing -> Nothing
eval1 (TermPred TermZero) = Just TermZero
eval1 (TermPred (TermSucc nv1)) = Just nv1
eval1 (TermPred t1) = case eval1 t1 of (Just t1') -> Just $ TermPred t1'
                                       Nothing -> Nothing
eval1 (TermIsZero TermZero) = Just TermTrue
eval1 (TermIsZero (TermSucc nv1)) = Just TermFalse
eval1 (TermIsZero t1) = case eval1 t1 of (Just t1') -> Just $ TermIsZero t1'
                                         Nothing -> Nothing
eval1 (TermApply (TermLambda name body) t) = Just $ substitute name t body
eval1 (TermApply f t) = case eval1 f of (Just f') -> Just $ TermApply f' t
                                        Nothing -> Nothing
eval1 _ = Nothing

eval :: Term -> Term
eval t = evalInt $ eval1 t where
    evalInt (Just t') = eval t'
    evalInt Nothing = t
