module Reliquary.Dictionary where

import Control.Monad
import Control.Monad.Except

import Reliquary.Utils.Error

import Reliquary.Core.AST

import Reliquary.AST

data SourceEntry = SourceEntry String Term
           deriving Show

type SourceDictionary = [SourceEntry]

data Entry = Entry { entryName :: String, entryTerm :: CoreTerm, entryType :: ([CoreTerm], [CoreTerm])}

type Dictionary = [Entry]

dictLookup :: Dictionary -> String -> Maybe Entry
dictLookup (e:es) n = if entryName e == n then Just e else dictLookup es n
dictLookup [] _ = Nothing

dictInsert :: Dictionary -> Entry -> Dictionary
dictInsert = flip (:)

numBreak :: [CoreTerm] -> [CoreTerm] -> Either GenError ([CoreTerm], [CoreTerm], [CoreTerm])
numBreak src i = go src i [] where
    go :: [CoreTerm] -> [CoreTerm] -> [CoreTerm] -> Either GenError ([CoreTerm], [CoreTerm], [CoreTerm])
    go remain [] acc = pure (reverse acc, remain, [])
    go [] miss acc = pure (reverse acc, [], miss)
    go (a:as) (b:bs) acc = if matchTerm a b
                               then go as bs (a:acc)
                               else throwError $ Mismatch a b

takeCons :: Int -> CoreTerm -> CoreTerm -> CoreTerm
takeCons 0 _ bot = bot
takeCons n t bot = CCons (CFst t) $ takeCons (n - 1) (CSnd t) bot

dropCons :: Int -> CoreTerm -> CoreTerm
dropCons n t = iterate CSnd t !! n

wrapLambda :: [CoreTerm] -> CoreTerm -> CoreTerm
wrapLambda take = CLambda $ foldr CSigma CUnitType take

wrap :: [CoreTerm] -> [CoreTerm] -> [CoreTerm] -> CoreTerm -> CoreTerm
wrap input needed output t = wrapLambda input $ takeCons (length output) (CApply t (takeCons (length needed) (CVar 0) CUnit)) (dropCons (length needed) (CVar 0))

compose :: ([CoreTerm], [CoreTerm]) -> ([CoreTerm], [CoreTerm]) -> CoreTerm -> CoreTerm -> Either GenError (CoreTerm, ([CoreTerm], [CoreTerm]))
compose (it, ot) (it', ot') i o = do
        (take, drop, miss) <- numBreak ot it'
        pure (wrapLambda (it ++ miss) $ CApply (wrap (take ++ miss) it' (ot' ++ drop) o) (CApply (wrap (it ++ miss) it ot i) (CVar 0)), (it ++ miss, ot' ++ drop))
