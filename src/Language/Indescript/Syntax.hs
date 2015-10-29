{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Language.Indescript.Syntax where

import GHC.Generics
import Data.Annotation

data Literal = LInt    Int
             | LFloat  Float
             | LChar   Char
             | LString String
             | LUnit
             deriving (Eq, Show)

data Variable = VarId  String
              | ConId  String
              | VarSym String
              | ConSym String
              deriving (Eq, Show)

data Expr a = ELam a
            | ELet a
            | EIf  a
            | ECase a
            | EApp (Expr a) [Expr a] a
            | EVar Variable a
            | ECon Variable a
            | EOp  Variable a
            | ELit Literal  a
            | EList a
            | ETuple a
            | ELOpSec (Expr a) (Expr a) a
            | EROpSec (Expr a) (Expr a) a
            | EInfix  (Expr a) (Expr a) (Expr a) a
            | ENeg    (Expr a) a
            deriving (Eq, Show, Functor, Generic1)

data EVar = EVVar String
          | EVOp  String
          deriving (Eq, Ord, Show)
data EOp  = EOVar String
          | EOOp  String
          deriving (Eq, Ord, Show)

data EAtom = A deriving (Eq, Show)

data Pattern a -- Atoms and Underscore
               = PAtom    EAtom a
               | PDiscard       a
               -- Constructors and @-Binding
               | PCon     (EVar, a) [Pattern a]             a
               | PConOp   (EOp, a)  (Pattern a) (Pattern a) a
               | PBinding (EVar, a) (Pattern a)             a
               deriving (Eq, Show, Functor, Generic1)

instance Annotation Pattern where
  annotation = genericAnnotation

data Branch a = Branch (Pattern a) (Expr a) a
              deriving (Eq, Show, Functor, Generic1)

instance Annotation Branch where
  annotation = genericAnnotation

-- "Supercombinator" here only refers to functions with
-- one or more cases.
-- [Supercomb a] stands for where-binding.
data Equation a  = EquFn (EVar, a) [Pattern a]             (Expr a) [Supercomb a] a
                 | EquOp (EOp, a)  (Pattern a) (Pattern a) (Expr a) [Supercomb a] a
                 deriving (Eq, Show, Functor, Generic1)
type Supercomb a = [Equation a]

instance Annotation Equation where
  annotation = genericAnnotation

data OpSecDir = SecLeft | SecRight deriving (Eq, Show)

{-
data Expr a = EAtom  EAtom a
            -- Operator Section/Application, Function Application, If-Then-Else
            | EOpSec (EOp, a) (Expr a) OpSecDir a
            | EOpApp (EOp, a) (Expr a) (Expr a) a
            | EApp   (Expr a) [Expr a]          a
            | EIf    (Expr a) (Expr a) (Expr a) a
            -- Case-Of
            | ECase  (Expr a) [Branch a] a
            -- Lambda
            | ELam   [Pattern a] (Expr a) a
            -- Let
            | ELet   [Supercomb a] (Expr a) a
            deriving (Eq, Show, Functor, Generic1)
-}

instance Annotation Expr where
  annotation = genericAnnotation

-- TODO: Redesign type repns.
data Type = TLit    TVar
          | TVar    TVar
          | TCon    TCon
          | TApp    Type Type
          | TForall [TVar] Type
          deriving (Eq, Show)
data TVar = TVStr String
          | TVSym String
          deriving (Eq, Ord, Show)
type TCon    = (TVar, Int)

-- TODO: Redesign those things' repns.
{-
data AssocType = Infix | InfixL | InfixR deriving (Eq, Show)
data Fixity a  = Fixity a AssocType Int  deriving (Eq, Show, Functor)

data ADTDecl a = ADTDecl TCon [ADTCon a] a deriving (Eq, Show, Functor)
data ADTCon  a = ADTCon  ECon [Type]     a deriving (Eq, Show, Functor)

data Decl a = FixityDecl   (Fixity a)    a
            | FuncDecl     (Supercomb a) a
            | DataTypeDecl (ADTDecl a)   a
            deriving (Eq, Show, Functor)
-}

updateAST :: Functor f => b -> f a -> f b
updateAST x' = fmap $ const x'

purifyAST :: Functor f => f a -> f ()
purifyAST = updateAST ()
