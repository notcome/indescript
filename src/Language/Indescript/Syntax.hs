{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Language.Indescript.Syntax where

import GHC.Generics    hiding (Fixity)
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

data Op a = Op Variable a
          deriving (Eq, Show, Functor, Generic1)

--   ## Expression
data Expr a = ELam a
            | ELet a
            | EIf  a
            | ECase a
            | EApp (Expr a) [Expr a] a
            | EVar Variable a
            | ECon Variable a
            | ELit Literal  a
            | ELOpSec (Op a) (Expr a) a
            | EROpSec (Op a) (Expr a) a
            | EInfix  (Expr a) (Op a) (Expr a) a
            | ENeg    (Expr a) a
            deriving (Eq, Show, Functor, Generic1)

instance Annotation Expr where
  annotation = genericAnnotation

--   ## Pattern
data Pat a = PVar Variable a
           | PAs  (Pat a) (Pat a) a
           | PLit Literal  a
           | PCon Variable a
           | PWildcard a
           | PApp   (Pat a) [Pat a] a
           | PInfix (Pat a) (Op a) (Pat a) a
           deriving (Eq, Show, Functor, Generic1)

instance Annotation Pat where
  annotation = genericAnnotation

--   ## Type
data Type a = TVar Variable a
            | TCon Variable a
            | TApp    (Type a) [Type a] a
            | TInfix  (Type a) (Op a) (Type a) a
            | TForall [Type a] (Type a) a
            deriving (Eq, Show, Functor, Generic1)

instance Annotation Type where
  annotation = genericAnnotation

--   ## Declaration
data AssocType = Infix | InfixL | InfixR deriving (Eq, Show)
data Fixity a  = Fixity   AssocType     [Op a] a
               | FixityLv AssocType Int [Op a] a
               deriving (Eq, Show, Functor, Generic1)

instance Annotation Fixity where
  annotation = genericAnnotation

data Var a = Var Variable a
           deriving (Eq, Show, Functor, Generic1)

instance Annotation Var where
  annotation = genericAnnotation

data FnLhs a = FnArgs (Var a) [Pat a] a
             | FnOp   (Pat a) (Op a) (Pat a) a
             deriving (Eq, Show, Functor, Generic1)

instance Annotation FnLhs where
  annotation = genericAnnotation

data Decl a = DeclFn (FnLhs a) (Expr a) [Decl a] a
            | DeclTypeSig [Var a] (Type a) a
            | DeclFixity  (Fixity a) a
            deriving (Eq, Show, Functor, Generic1)

instance Annotation Decl where
  annotation = genericAnnotation

-- "Supercombinator" here only refers to functions with
-- one or more cases.
-- [Supercomb a] stands for where-binding.
{-
data Equation a  = EquFn (EVar, a) [Pattern a]             (Expr a) [Supercomb a] a
                 | EquOp (EOp, a)  (Pattern a) (Pattern a) (Expr a) [Supercomb a] a
                 deriving (Eq, Show, Functor, Generic1)
type Supercomb a = [Equation a]

instance Annotation Equation where
  annotation = genericAnnotation
-}

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
