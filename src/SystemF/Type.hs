module SystemF.Type where

data Type = TypeBool
          | TypeNat
          | TypeArrow Type Type
          | TypeVar String
          | TypeApply Type Type
          | TypeLambda String Kind Type
          deriving (Show, Eq)

data Kind = KindType
          | KindArrow Kind Kind
          deriving (Show, Eq)

data TypeError = Mismatch Type Type
               | NotInScope String
               | NotFunction Type
               | InvalidTypeApplication
               deriving Show

data KindError = KindMismatch Kind Kind
               | TypeNotInScope String
               | NotTypeFunction Kind
               deriving Show

type Env = [(String, Type)]
type TEnv = [(String, Kind)]
