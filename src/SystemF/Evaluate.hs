module SystemF.Evaluate where

import SystemF.AST
import qualified Lambda.AST as L

translate :: Term -> L.Term
translate TermTrue = L.TermTrue
translate TermFalse = L.TermFalse
translate (TermIf t1 t2 t3) = L.TermIf (translate t1)
                                       (translate t2)
                                       (translate t3)
translate TermZero = L.TermZero
translate (TermSucc t) = L.TermSucc $ translate t
translate (TermPred t) = L.TermPred $ translate t
translate (TermIsZero t) = L.TermIsZero $ translate t
translate (TermVar n) = L.TermVar n
translate (TermApply f t) = L.TermApply (translate f)
                                        (translate t)
translate (TermLambda n ntype t) = L.TermLambda n $ translate t
translate (TermTypeApply t ttype) = translate t
translate (TermTypeLambda n nkind t) = translate t
