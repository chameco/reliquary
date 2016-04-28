module CodeGen where

import Control.Monad.State

import Model

type CodeGenState = (String, String, String, String)

genData :: String -> State CodeGenState ()
genData s = do
    (d, b, f, t) <- get
    put (d ++ s, b, f, t)

genBSS :: String -> State CodeGenState ()
genBSS s = do
    (d, b, f, t) <- get
    put (d, b ++ s, f, t)

genFunc :: String -> State CodeGenState ()
genFunc s = do
    (d, b, f, t) <- get
    put (d, b, f ++ s, t)

genText :: String -> State CodeGenState ()
genText s = do
    (d, b, f, t) <- get
    put (d, b, f, t ++ s)

genFuncDef :: String -> Expr -> State CodeGenState ()
genFuncDef name body = genFunc
        $ "global " ++ name ++ "\n"
        ++ name ++ ":\n"
        ++ "pop rdx\n"
        ++ getText (runState (generate body) ("", "", "", ""))
        ++ "push rdx\n"
        ++ "ret\n"
    where getText ((), (d, b, f, t)) = t

generateBlock :: Expr -> State CodeGenState ()
generateBlock (Word s) = genText $ "push " ++ s ++ "\n"

generate :: Expr -> State CodeGenState ()
generate (Program xs) = foldr1 (>>) $ map generate xs
generate (Word s) = genText $ "call " ++ s ++ "\n"
generate (Literal i) = genText $ "push " ++ show i ++ "\n"
generate (Block bs) = foldr1 (>>) $ map generateBlock bs
generate (Definition name body) = genFuncDef name body
generate (Import name) = genFunc $ "extern " ++ name ++ "\n"

buildOutput :: ((), CodeGenState) -> String
buildOutput ((), (d, b, f, t)) =
    "section .data\n" ++ d ++
    "section .bss\n" ++ b ++
    "section .text\n" ++ f ++
    "global _start\n_start:\n" ++ t ++ "call exit\n"
