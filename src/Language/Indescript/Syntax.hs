{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Language.Indescript.Syntax where

newtype Fix w f = In { out :: w (f (Fix w f)) }

type Outer w f = f (Fix w f)
type Inner w f = Fix w f

data Lit = LInt    Int
         | LFloat  Float
         | LChar   Char
         | LString String
         | LUnit
         deriving (Eq, Show)

data Var = VarId  String
         | ConId  String
         | VarSym String
         | ConSym String
         deriving (Eq, Show)

type Expr w = Fix w (ExprF w)
data ExprF w f = EVar Var
               | ECon Var
               | ELit Lit
               | ENeg   f
               | EParen f
               | ELOpSec (w Var) f
               | EROpSec (w Var) f
               | EInfix  f (w Var) f
               | EApp    f [f]
               | EAnn  f (Type w)
               | EIf   f f f
               | ELam  [Pat w] f
               | ELet  [Decl w] f
               | ECase f [(Pat w, f, [Decl w])]
               deriving (Functor)

type Type w = Fix w (TypeF w)
data TypeF w f = TVar Var
               | TCon Var
               | TParen f
               | TInfix f (w Var) f
               | TApp   f [f]
               | TScheme [w Var] f
               deriving (Functor)

type Pat w = Fix w (PatF w)
data PatF w f = PVar Var
              | PLit Lit
              | PParen f
              | PAs  (w Var) f
              | PWildcard
              | PInfix  f (w Var) f
              | PConApp (w Var) [f]
              deriving (Functor)

type Decl w = Fix w (DeclF w)
data DeclF w f = DeclEqt  (w (FnLhs w)) (Expr w) [f]
               | DeclEqts [f]
               | DeclFixity    AssocType Int [w Var]
               | DeclTypeSig   [w Var] (Type w)
               | DeclTypeAlias (Type w) (Type w)
               | DeclNewType   (Type w) (Type w) (Type w)
               | DeclDataType  (Type w) [Type w]
               deriving (Functor)

data FnLhs w = FnArgs (w Var) [Pat w]
             | FnOp   (Pat w) (w Var) (Pat w)

data AssocType = Infix | InfixL | InfixR deriving (Eq, Show)
