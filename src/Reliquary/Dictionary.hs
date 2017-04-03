module Reliquary.Dictionary where

import Control.Monad
import Control.Monad.Except

import Reliquary.Core.AST

import Reliquary.AST

data SourceEntry = SourceEntry String Term
           deriving Show

type SourceDictionary = [SourceEntry]

type TypedFunction = (CoreTerm, ([CoreTerm], [CoreTerm]))
type Dictionary = [(String, TypedFunction)]

displayFunc :: TypedFunction -> String
displayFunc (t, (ity, oty)) = displayTerm t ++ " : {" ++ joinAll ity ++ "} -> {" ++ joinAll oty ++ "}"
    where joinAll [] = ""
          joinAll ts = foldr1 join $ map displayTerm ts
          join x y = x ++ ", " ++ y

dictLookup :: Dictionary -> String -> Maybe TypedFunction
dictLookup (e:es) n = if fst e == n then Just $ snd e else dictLookup es n
dictLookup [] _ = Nothing

dictInsert :: Dictionary -> (String, TypedFunction) -> Dictionary
dictInsert = flip (:)

numBreak :: [CoreTerm] -> [CoreTerm] -> Either GenError ([CoreTerm], [CoreTerm], [CoreTerm])
numBreak src i = go src i [] where
    go :: [CoreTerm] -> [CoreTerm] -> [CoreTerm] -> Either GenError ([CoreTerm], [CoreTerm], [CoreTerm])
    go remain [] acc = return (reverse acc, remain, [])
    go [] miss acc = return (reverse acc, [], miss)
    go (a:as) (b:bs) acc = if matchTerm a b then go as bs (a:acc) else throwError $ Mismatch a b
takeCons 0 _ bot = bot
takeCons n t bot = CCons (CFst t) $ takeCons (n - 1) (CSnd t) bot
dropCons n t = iterate CSnd t !! n
wrapLambda take = CLambda $ foldr CSigma CUnitType take
wrap :: [CoreTerm] -> [CoreTerm] -> [CoreTerm] -> CoreTerm -> CoreTerm
wrap input needed output t = wrapLambda input $ takeCons (length output) (CApply t (takeCons (length needed) (CVar 0) CUnit)) (dropCons (length needed) (CVar 0))

compose :: TypedFunction -> TypedFunction -> Either GenError TypedFunction
compose (i, (it, ot)) (o, (it', ot')) = do
        (take, drop, miss) <- numBreak ot it'
        return (wrapLambda (it ++ miss) $ CApply (wrap (ot ++ miss) it' ot' o) (CApply (wrap (it ++ miss) it ot i) (CVar 0)), (it ++ miss, ot' ++ drop))

composeAll :: [TypedFunction] -> Either GenError TypedFunction
composeAll [] = return (CLambda CUnitType CUnit, ([], []))
composeAll [t] = return t
composeAll (t:ts) = composeAll ts >>= compose t
