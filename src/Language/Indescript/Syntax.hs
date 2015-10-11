{-# LANGUAGE DeriveFunctor #-}
module Language.Indescript.Syntax where

import Data.Functor

data Expr a = ELit   Literal a
            | EVar   EVar a
            | ECon   ECon a
            | EApp   (Expr a) [Expr a] a
            | EIf    (Expr a) (Expr a) (Expr a) a
            | ECase  (Expr a) [Branch a] a
            | ELam   [Pattern a] (Expr a) a
            | ELet   [Supercomb a] (Expr a) a
            | EWhere [Supercomb a] a
            deriving (Eq, Show, Functor)

data Branch a = Branch (Pattern a) (Expr a) a
              deriving (Eq, Show, Functor)

data Pattern a = PLit Literal a
               | PVar EVar a
               | PCon ECon [Pattern a] a
               | PAt  EVar (Pattern a) a
               deriving (Eq, Show, Functor)

data Supercomb a = SCConst EVar (Expr a) a
                 | SCFun   EVar [Pattern a] (Expr a) a
                 deriving (Eq, Show, Functor)

data Type = TLit    TVar
          | TVar    TVar
          | TCon    TCon
          | TApp    Type Type
          | TForall [TVar] Type
          deriving (Eq, Show)

newtype EVar = LEVar String deriving (Eq, Ord, Show)
newtype TVar = LTVar String deriving (Eq, Ord, Show)
type ECon    = (EVar, Int)
type TCon    = (TVar, Int)

data Literal = LInt    Int
             | LFloat  Float
             | LChar   Char
             | LString String
             deriving (Eq, Show)

data AssocType = Infix | InfixL | InfixR deriving (Eq, Show)
data Fixity a  = Fixity a AssocType Int  deriving (Eq, Show, Functor)

data ADTDecl a = ADTDecl TCon [ADTCon a] a deriving (Eq, Show, Functor)
data ADTCon  a = ADTCon  ECon [Type]     a deriving (Eq, Show, Functor)

data Decl a = FixityDecl   (Fixity a)    a
            | FuncDecl     (Supercomb a) a
            | DataTypeDecl (ADTDecl a)   a
            deriving (Eq, Show, Functor)

purifyAST :: Functor f => f a -> f ()
purifyAST = fmap $ const ()
