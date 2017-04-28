module Reliquary.Dictionary where

import Control.Monad
import Control.Monad.Except

import Reliquary.Core.AST
import Reliquary.Core.DeBruijn
import Reliquary.Core.TypeCheck

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

dictInsert :: Dictionary -> (String, Typed) -> Dictionary
dictInsert = flip (:)

-- Given two sigma chains, find the "difference" between those chains for
-- the purpose of determining final input/output types of a composition
numBreak :: CoreTerm -> CoreTerm -> Either GenError (CoreTerm, CoreTerm)
numBreak remain CUnitType = return (remain, CUnitType)
numBreak CUnitType miss = return (CUnitType, miss)
numBreak (CSigma a as) (CSigma b bs) = numBreak as bs
numBreak _ _ = throwError $ InternalError CUnit

-- Given two sigma term chains, append those chains
appendSigma :: CoreTerm -> CoreTerm -> Either GenError CoreTerm
appendSigma CUnitType t' = return t'
appendSigma (CSigma t ts) t' = CSigma t . shift 1 <$> appendSigma ts t'
appendSigma o _ = throwError $ InternalError o

-- Find the length of a chain of sigma terms
lengthSigma :: CoreTerm -> Int
lengthSigma (CSigma _ t) = 1 + lengthSigma t
lengthSigma _ = 0

-- Generate an expression extracting some number of elements from a list
takeCons :: Int -> CoreTerm -> CoreTerm -> CoreTerm
takeCons 0 _ bot = bot
takeCons n t bot = CCons (CFst t) $ takeCons (n - 1) (CSnd t) bot

-- Generate an expression dropping some number of elements from a list
dropCons :: Int -> CoreTerm -> CoreTerm
dropCons n t = iterate CSnd t !! n

-- Transform a sigma term chain into nested lambda terms about a given term
curryLambda :: CoreTerm -> CoreTerm -> CoreTerm
curryLambda (CSigma it rest) bot = CLambda it $ shift 1 $ curryLambda rest bot
curryLambda CUnitType bot = CLambda CUnitType $ shift 1 bot
curryLambda _ _ = CUnit -- Internal error

-- Transform a sigma term into nested pi terms about a given term
curryPi :: CoreTerm -> CoreTerm -> CoreTerm
curryPi (CSigma it rest) bot = CPi it $ shift 1 $ curryPi rest bot
curryPi CUnitType bot = CPi CUnitType $ shift 1 bot
curryPi _ _ = CUnit -- Internal error

uncurryPi :: CoreTerm -> CoreTerm
uncurryPi (CPi CUnitType _) = CUnitType
uncurryPi (CPi it rest) = CSigma it $ shift 1 $ uncurryPi rest
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

compose :: Dictionary -> Typed -> Typed -> Either GenError Typed
compose d (i, ic@(CPi it ot)) (o, oc@(CPi it' ot')) = do
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

        check [] $ dictWrap d oresult 
        ty <- check [] $ dictWrap d $ curryLambda finalInput oenriched
        return (curryLambda finalInput oenriched, ty)
compose _ (i, _) (_, CPi{}) = throwError $ NotFunction i
compose _ (_, _) (o, _) = throwError $ NotFunction o

composeAll :: Dictionary -> [Typed] -> Either GenError Typed
composeAll d = foldM (compose d) (CLambda CUnitType CUnit, CPi CUnitType CUnitType)

-- Generate let bindings from a dictionary around a provided term
dictWrap :: Dictionary -> CoreTerm -> CoreTerm
dictWrap [] t' = t'
dictWrap ((_, (t, ty)):ds) t' = CApply (CLambda ty (dictWrap ds t')) t
