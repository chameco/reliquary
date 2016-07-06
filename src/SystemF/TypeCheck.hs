module SystemF.TypeCheck where

import SystemF.AST
import SystemF.Type

typeSubstitute :: String -> Type -> Type -> Type
typeSubstitute _ _ TypeBool = TypeBool
typeSubstitute _ _ TypeNat = TypeNat
typeSubstitute name new (TypeArrow pre post) = TypeArrow (typeSubstitute name new pre)
                                                         (typeSubstitute name new post)
typeSubstitute name new (TypeUniversal name' t)
    | name == name' = TypeUniversal name' t
    | otherwise = TypeUniversal name' (typeSubstitute name new t)
typeSubstitute name new (TypeVar name')
    | name == name' = new
    | otherwise = TypeVar name'

checkType :: Env -> Term -> Either TypeError Type
checkType _ TermTrue = Right TypeBool
checkType _ TermFalse = Right TypeBool
checkType env (TermIf c t e) = do
        tc <- checkType env c
        case tc of
            TypeBool -> do
                tt <- checkType env t
                te <- checkType env e
                if tt /= te then Left $ Mismatch tt te else Right tt
            _ -> Left $ Mismatch TypeBool tc
checkType _ TermZero = Right TypeNat
checkType env (TermSucc t) = do
        tt <- checkType env t
        if tt /= TypeNat then Left $ Mismatch TypeNat tt else Right TypeNat
checkType env (TermPred t) = do
        tt <- checkType env t
        if tt /= TypeNat then Left $ Mismatch TypeNat tt else Right TypeNat
checkType env (TermIsZero t) = do
        tt <- checkType env t
        if tt /= TypeNat then Left $ Mismatch TypeNat tt else Right TypeBool
checkType env (TermVar name) = case lookup name env of
                                   Just t -> Right t
                                   Nothing -> Left $ NotInScope name
checkType env (TermApply f t) = do
        tf <- checkType env f
        tt <- checkType env t
        case tf of TypeArrow pre post -> if tt /= pre then Left $ Mismatch pre tt else Right post
                   _ -> Left $ NotFunction tf
checkType env (TermLambda name ntype t) = do
        tt <- checkType ((name, ntype):env) t
        Right $ TypeArrow ntype tt
checkType env (TermTypeApply t new) = do
        tt <- checkType env t
        case tt of TypeUniversal name t' -> Right $ typeSubstitute name new t'
                   _ -> Left InvalidTypeApplication
checkType env (TermTypeLambda name t) = do
        tt <- checkType env t
        Right $ TypeUniversal name tt
