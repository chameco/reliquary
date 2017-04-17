module Reliquary.Dictionary where

import Control.Monad
import Control.Monad.Except

import Reliquary.Core.AST
import Reliquary.Core.DeBruijn

import Reliquary.AST

data SourceEntry = SourceEntry String Term
           deriving Show

type SourceDictionary = [SourceEntry]

type Typed = (CoreTerm, CoreTerm)
type Dictionary = [(String, Typed)]

displayTyped :: Typed -> String
displayTyped (t, ty) = displayTerm t ++ " : " ++ displayTerm ty

dictLookup :: Dictionary -> String -> Maybe Typed
dictLookup (e:es) n = if fst e == n then Just $ snd e else dictLookup es n
dictLookup [] _ = Nothing

unsafeLookup :: Dictionary -> String -> CoreTerm
unsafeLookup d n = case dictLookup d n of Nothing -> error "Internal error"; Just t -> snd t

dictInsert :: Dictionary -> (String, Typed) -> Dictionary
dictInsert = flip (:)

numBreak :: CoreTerm -> CoreTerm -> Either GenError (CoreTerm, CoreTerm, CoreTerm)
numBreak src i = go src i CUnitType where
    go :: CoreTerm -> CoreTerm -> CoreTerm -> Either GenError (CoreTerm, CoreTerm, CoreTerm)
    go remain CUnitType acc = return (acc, remain, CUnitType)
    go CUnitType miss acc = return (acc, CUnitType, miss)
    go (CSigma a as) (CSigma b bs) acc = do
        newAcc <- appendSigma acc a
        if matchTerm a b then go as bs newAcc else throwError $ Mismatch a b
    go _ _ _ = throwError $ InternalError CStar

takeCons :: Int -> CoreTerm -> CoreTerm -> CoreTerm
takeCons 0 _ bot = bot
takeCons n t bot = CCons (CFst t) $ takeCons (n - 1) (CSnd t) bot

dropCons :: Int -> CoreTerm -> CoreTerm
dropCons n t = iterate CSnd t !! n

wrap :: CoreTerm -> CoreTerm -> CoreTerm -> CoreTerm -> CoreTerm
wrap input needed output t = CLambda input $ shift 1 $ takeCons (lengthSigma output) (CApply t (takeCons (lengthSigma needed) (CVar (-1)) CUnit)) (dropCons (lengthSigma needed) (CVar (-1)))

appendSigma :: CoreTerm -> CoreTerm -> Either GenError CoreTerm
appendSigma CUnitType t' = return t'
appendSigma (CSigma t ts) t' = CSigma t <$> appendSigma ts t'
appendSigma _ _ = throwError $ InternalError CStar

lengthSigma :: CoreTerm -> Int
lengthSigma (CSigma _ t) = 1 + lengthSigma t
lengthSigma _ = 0

compose :: Typed -> Typed -> Either GenError Typed
compose (i, CPi it ot) (o, CPi it' ot') = do
        (take, drop, miss) <- numBreak ot it'
        finalInput <- appendSigma it miss
        wrapInput <- appendSigma ot miss
        finalOutput <- appendSigma ot' drop
        return (CLambda finalInput $ shift 1 $ CApply (wrap wrapInput it' ot' o) (CApply (wrap finalInput it ot i) (CVar (-1))), CPi finalInput finalOutput)
compose _ _ = throwError $ InternalError CStar

composeAll :: [Typed] -> Either GenError Typed
composeAll [] = return (CLambda CUnitType CUnit, CPi CUnitType CUnitType)
composeAll [t] = return t
composeAll (t:ts) = composeAll ts >>= compose t

dictWrap :: Dictionary -> CoreTerm -> CoreTerm
dictWrap [] t' = t'
dictWrap ((_, (t, ty)):ds) t' = CApply (CLambda ty (dictWrap ds t')) t
