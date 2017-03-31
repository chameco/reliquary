module Reliquary.Dictionary where

import Reliquary.Core.AST

import Reliquary.AST

data SourceEntry = SourceEntry String Term
           deriving Show

type SourceDictionary = [SourceEntry]

data Entry = Entry { entryName :: String, entryTerm :: CoreTerm, entryType :: CoreTerm }

type Dictionary = [Entry]

dictLookup :: Dictionary -> String -> Maybe Entry
dictLookup (e:es) n = if entryName e == n then Just e else dictLookup es n
dictLookup [] _ = Nothing

dictInsert :: Dictionary -> Entry -> Dictionary
dictInsert = flip (:)

toCoreEnv :: Dictionary -> CoreEnv
toCoreEnv = go 0 . reverse where
    go acc (e:es) = (entryType e, acc):go (acc + 1) es
    go _ [] = []
