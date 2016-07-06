module SystemF.Type where

data Type = TypeBool
          | TypeNat
          | TypeArrow Type Type
          | TypeUniversal String Type
          | TypeVar String
          deriving (Show, Eq)

data TypeError = Mismatch Type Type
               | NotInScope String
               | NotFunction Type
               | InvalidTypeApplication
               deriving Show

type Env = [(String, Type)]
