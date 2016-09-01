module Reliquary.Dictionary where

import Reliquary.AST

data Entry = Entry { entryName :: String, entryTerm :: Term, entryType :: Term }

type Dictionary = [Entry]

dictLookup :: Dictionary -> String -> Maybe Entry
dictLookup (e:es) n = if entryName e == n then Just e else dictLookup es n
dictLookup [] _ = Nothing

dictInsert :: Dictionary -> Entry -> Dictionary
dictInsert = flip (:)
