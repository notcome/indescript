module Language.Indescript.Parser where

import Language.Indescript.Lexer

data Expr = ELit   Literal
          | EVar   EVar
          | ECon   ECon
          | EApp   Expr [Expr]
          | EIf    Expr Expr Expr
          | ECase  Expr [(Pattern, Expr)]
          | ELam   [Pattern] Expr
          | ELet   [Binding] Expr
          | EWhere [Binding]
          deriving (Eq, Show)

type Binding = (EVar, Expr)

data Pattern = PLit Literal
             = PVar EVar
             = PCon Constructor [Pattern]

newtype EVar = LEVar String deriving (Eq, Ord, Show)
newtype TVar = LTVar String deriving (Eq, Ord, Show)
type ECon    = (EVar, Int)
type TCon    = (TVar, Int)

data Type = TLit TVar
          | TVar TVar
          | TCon TCon
          | TApp Type Type
          deriving (Eq, Show)

data Forall = Forall [Type] Type

data FuncTypeDecl = FuncTypeDecl EVar Forall
data FuncDecl     = FuncDecl EVar [Pattern] Expr
                  | OprDecl  EVar Pattern Pattern Expr

data Decl = FuncTypeDecl
          | FuncDecl
          | DataTypeDecl
          | FixityDecl
