module Reliquary.Dictionary where

import Control.Monad
import Control.Monad.Except

import Reliquary.Core.AST
import Reliquary.Core.DeBruijn

import Reliquary.AST

import Debug.Trace

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

numBreak :: CoreTerm -> CoreTerm -> Either GenError (CoreTerm, CoreTerm)
numBreak remain CUnitType = return (remain, CUnitType)
numBreak CUnitType miss = return (CUnitType, miss)
numBreak (CSigma a as) (CSigma b bs) = numBreak as bs

appendSigma :: CoreTerm -> CoreTerm -> Either GenError CoreTerm
appendSigma CUnitType t' = return t'
appendSigma (CSigma t ts) t' = CSigma t <$> appendSigma ts t'
appendSigma o _ = throwError $ InternalError o

lengthSigma :: CoreTerm -> Int
lengthSigma (CSigma _ t) = 1 + lengthSigma t
lengthSigma _ = 0

takeCons :: Int -> CoreTerm -> CoreTerm -> CoreTerm
takeCons 0 _ bot = bot
takeCons n t bot = CCons (CFst t) $ takeCons (n - 1) (CSnd t) bot

dropCons :: Int -> CoreTerm -> CoreTerm
dropCons n t = iterate CSnd t !! n

curryLambda :: CoreTerm -> CoreTerm -> CoreTerm
curryLambda (CSigma it rest) bot = CLambda it $ shift 1 $ curryLambda rest bot
curryLambda CUnitType bot = CLambda CUnitType bot
curryLambda _ _ = CUnit -- Internal error

curryPi :: CoreTerm -> CoreTerm -> CoreTerm
curryPi (CSigma it rest) bot = CPi it $ shift 1 $ curryPi rest bot
curryPi CUnitType bot = CPi CUnitType bot
curryPi _ _ = CUnit -- Internal error

uncurryPi :: CoreTerm -> CoreTerm
uncurryPi (CPi CUnitType _) = CUnitType
uncurryPi (CPi it rest) = CSigma it $ uncurryPi rest
uncurryPi _ = CUnitType

uncurryRest :: CoreTerm -> CoreTerm
uncurryRest (CPi it rest) = uncurryRest rest
uncurryRest rest = rest

buildArgCons :: Int -> CoreTerm
buildArgCons i | i <= 1 = CUnit | otherwise = CCons (CVar (i - 1)) $ buildArgCons (i - 1)

applyLambda :: Int -> CoreTerm -> CoreTerm
applyLambda 0 bot = CApply bot CUnit
applyLambda i bot = applyLambda (i - 1) $ CApply bot $ CVar (i - 1)

applyCons :: Int -> CoreTerm -> CoreTerm -> CoreTerm
applyCons 0 term bot = CApply bot CUnit
applyCons i term bot = applyCons (i - 1) (CSnd term) $ CApply bot $ CFst term

compose :: Typed -> Typed -> Either GenError Typed
compose (i, ic@(CPi it ot)) (o, oc@(CPi it' ot')) = do
        let lenit = lengthSigma $ uncurryPi ic
            lenot = lengthSigma $ uncurryRest ic
            lenit' = lengthSigma $ uncurryPi oc
            lenot' = lengthSigma $ uncurryRest oc

        (drop, miss) <- numBreak (uncurryRest ic) (uncurryPi oc)
        finalInput <- appendSigma (uncurryPi ic) miss
        finalOutput <- appendSigma (uncurryRest oc) drop

        let iresult = applyLambda lenit i
            ienriched = takeCons lenot iresult $ buildArgCons lenit
            oresult = applyCons lenit' ienriched o
            oenriched = takeCons lenot' oresult $ dropCons lenit' ienriched

        return (curryLambda finalInput $ shift 1 oenriched, curryPi finalInput finalOutput)
compose (i, _) (_, CPi{}) = throwError $ NotFunction i
compose (_, _) (o, _) = throwError $ NotFunction o

composeAll :: [Typed] -> Either GenError Typed
composeAll = foldM compose (CLambda CUnitType CUnit, CPi CUnitType CUnitType)

dictWrap :: Dictionary -> CoreTerm -> CoreTerm
dictWrap [] t' = t'
dictWrap ((_, (t, ty)):ds) t' = CApply (CLambda ty (dictWrap ds t')) t
