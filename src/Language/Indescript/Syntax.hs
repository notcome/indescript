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
data Alt a = EAlt (Pat a) (Expr a) [Decl a] a
           deriving (Eq, Show, Functor, Generic1)

instance Annotation Alt where
  annotation = genericAnnotation

data Expr a = ELam [Pat a]  (Expr a) a
            | ELet [Decl a] (Expr a) a
            | EIf  (Expr a) (Expr a) (Expr a) a
            | ECase (Expr a) [Alt a] a
            | EApp (Expr a) [Expr a] a
            | EVar Variable a
            | ECon Variable a
            | ELit Literal  a
            | ELOpSec (Op a) (Expr a) a
            | EROpSec (Op a) (Expr a) a
            | EInfix  (Expr a) (Op a) (Expr a) a
            | ENeg    (Expr a) a
-- Intermediate representation, removed after resolving infix expressions.
            | EParen  (Expr a) a
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
            | DeclTypeAlias (Type a) (Type a) a
            | DeclNewType   (Type a) (Type a) (Type a) a
            | DeclDataType  (Type a) [Type a] a
            deriving (Eq, Show, Functor, Generic1)

instance Annotation Decl where
  annotation = genericAnnotation

updateAST :: Functor f => b -> f a -> f b
updateAST x' = fmap $ const x'

purifyAST :: Functor f => f a -> f ()
purifyAST = updateAST ()
