module SystemF.TypeCheck where

import SystemF.AST
import SystemF.Type

typeSubstitute :: String -> Type -> Type -> Type
typeSubstitute _ _ TypeBool = TypeBool
typeSubstitute _ _ TypeNat = TypeNat
typeSubstitute name new (TypeArrow pre post) = TypeArrow (typeSubstitute name new pre)
                                                         (typeSubstitute name new post)
typeSubstitute name new (TypeVar name')
    | name == name' = new
    | otherwise = TypeVar name'
typeSubstitute name new (TypeApply f t) = TypeApply (typeSubstitute name new f)
                                                    (typeSubstitute name new t)
typeSubstitute name new (TypeLambda name' nkind t)
    | name == name' = TypeLambda name' nkind t
    | otherwise = TypeLambda name' nkind (typeSubstitute name new t)

typeEval1 :: Type -> Maybe Type
typeEval1 (TypeApply (TypeLambda name nkind body) t) = Just $ typeSubstitute name t body
typeEval1 (TypeApply f t) = case typeEval1 f of (Just f') -> Just $ TypeApply f' t
                                                Nothing -> Nothing
typeEval1 _ = Nothing

typeEval :: Type -> Type
typeEval t = typeEvalInt $ typeEval1 t where
    typeEvalInt (Just t') = typeEval t'
    typeEvalInt Nothing = t

checkType :: Env -> TEnv -> Term -> Either TypeError Type
checkType _ _ TermTrue = Right TypeBool
checkType _ _ TermFalse = Right TypeBool
checkType env tenv (TermIf c t e) = do
        tc <- checkType env tenv c
        case tc of
            TypeBool -> do
                tt <- checkType env tenv t
                te <- checkType env tenv e
                if tt /= te then Left $ Mismatch tt te else Right tt
            _ -> Left $ Mismatch TypeBool tc
checkType _ _ TermZero = Right TypeNat
checkType env tenv (TermSucc t) = do
        tt <- checkType env tenv t
        if tt /= TypeNat then Left $ Mismatch TypeNat tt else Right TypeNat
checkType env tenv (TermPred t) = do
        tt <- checkType env tenv t
        if tt /= TypeNat then Left $ Mismatch TypeNat tt else Right TypeNat
checkType env tenv (TermIsZero t) = do
        tt <- checkType env tenv t
        if tt /= TypeNat then Left $ Mismatch TypeNat tt else Right TypeBool
checkType env tenv (TermVar name) = case lookup name env of
                                        Just t -> Right t
                                        Nothing -> Left $ NotInScope name
checkType env tenv (TermApply f t) = do
        tf <- checkType env tenv f
        tt <- checkType env tenv t
        case tf of TypeArrow pre post -> if tt /= pre then Left $ Mismatch pre tt else Right post
                   _ -> Left $ NotFunction tf
checkType env tenv (TermLambda name ntype t) = do
        tt <- checkType ((name, ntype):env) tenv t
        Right $ TypeArrow ntype tt
checkType env tenv (TermTypeApply t new) = do
        tt <- checkType env tenv t
        case tt of TypeLambda name nkind t' -> Right $ typeSubstitute name new t'
                   _ -> Left InvalidTypeApplication
checkType env tenv (TermTypeLambda name nkind t) = do
        tt <- checkType env tenv t
        Right $ TypeLambda name nkind tt

checkKind :: TEnv -> Type -> Either KindError Kind
checkKind _ TypeBool = Right KindType
checkKind _ TypeNat = Right KindType
checkKind _ (TypeArrow _ _) = Right KindType
checkKind tenv (TypeVar name) = case lookup name tenv of
                                    Just k -> Right k
                                    Nothing -> Left $ TypeNotInScope name
checkKind tenv (TypeApply f t) = do
        kf <- checkKind tenv f
        kt <- checkKind tenv t
        case kf of KindArrow pre post -> if kt /= pre then Left $ KindMismatch pre kt else Right post
                   _ -> Left $ NotTypeFunction kf
checkKind tenv (TypeLambda name nkind t) = do
        kt <- checkKind ((name, nkind):tenv) t
        Right $ KindArrow nkind kt
