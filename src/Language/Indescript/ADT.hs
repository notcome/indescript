module Language.Indescript.ADT where

data Exp = EVar EVar
         | ELit ELit
         | EApp Exp  Exp     -- f x
         | EAbs EVar Exp     -- \v.e
         | EIf  Exp  Exp Exp -- if x then l else r
         | ELet EVar Exp Exp -- fix (\f.e1) in e2
         deriving (Eq, Show)

newtype EVar = LEVar String deriving (Eq, Ord, Show)
newtype TVar = LTVar String deriving (Eq, Ord, Show)

data ELit = LBool Bool
          | LInt Int
          deriving (Eq, Show)

data Type = TBool
          | TInt
          | TArr Type Type
          | TVar TVar
          deriving (Eq, Show)
