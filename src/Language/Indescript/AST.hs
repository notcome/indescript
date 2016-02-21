{-# OPTIONS_GHC -ddump-splices #-}

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TupleSections   #-}

module Language.Indescript.AST where

import Data.Proxy

import Control.SemiIso
import Control.SemiIso.TH

-- import Control.IxFix

data Name = Name String
data Lit  = Num  Integer

-- ####
--    # Type Indexes and Proxies
data AstIx = Decl | Expr | Patt | Type

--   ## Type Synonyms for Promoted Term Constrcutors
type Decl = 'Decl
type Expr = 'Expr
type Patt = 'Patt
type Type = 'Type

--    # AST
data Ast i where
--   ## Atomic AST Nodes (Expr, Patt, Decl)
  Var   :: Proxy i -> Name -> Ast i
  Con   :: Proxy i -> Name -> Ast i
  Lit   :: Proxy i -> Lit -> Ast i
  Paren :: Ast i -> Ast i
--   ## Complex AST Expression
  App   :: Ast i -> [Ast i] -> Ast i
  Infix :: Ast i -> Ast i -> Ast i -> Ast i
  Neg   :: Ast Expr -> Ast Expr
  LOSec :: Ast Expr -> Ast Expr -> Ast Expr
  ROSec :: Ast Expr -> Ast Expr -> Ast Expr
  TyAnn :: Ast Expr -> Ast Type -> Ast Expr
  If    :: Ast Expr -> Ast Expr -> Ast Expr -> Ast Expr
  Lam   :: [Ast Patt] -> Ast Expr -> Ast Expr
  Let   :: Ast Decl -> Ast Expr -> Ast Expr
  Alt   :: Ast Patt -> Ast Expr -> Ast Decl -> Ast Expr
  Case  :: Ast Expr -> [Ast Expr] -> Ast Expr

$(deriveSemiIsos ''Ast)

type family PF adt :: (* -> *) -> (* -> *)

-- (DataD [] Ast [KindedTV ix (ConT AstIx)]
--   [NormalC Var
--     [(NotStrict,AppT (ConT Proxy) (VarT ix)),(NotStrict,ConT Name)],
--
--   NormalC Con [(NotStrict,AppT (ConT Proxy) (VarT ix)),(NotStrict,ConT Name)],
--
--   NormalC Lit [(NotStrict,AppT (ConT Proxy) (VarT ix)),(NotStrict,ConT Lit)],
--
--   NormalC Paren [(NotStrict,AppT (ConT Ast) (VarT ix))],NormalC App [(NotStrict,AppT (ConT Ast) (VarT ix)),(NotStrict,AppT ListT (AppT (ConT Ast) (VarT ix)))],
--
--   NormalC Infix [(NotStrict,AppT (ConT Ast) (VarT ix)),(NotStrict,AppT (ConT Ast) (VarT ix)),(NotStrict,AppT (ConT Ast) (VarT ix))],
--
--   ForallC [] [AppT (AppT EqualityT (VarT ix)) (ConT Expr)]
--     (NormalC Neg [(NotStrict,AppT (ConT Ast) (ConT Expr))]),
--
--   ForallC [] [AppT (AppT EqualityT (VarT ix)) (ConT Expr)]
--     (NormalC LOSec [(NotStrict,AppT (ConT Ast) (ConT Expr)),(NotStrict,AppT (ConT Ast) (ConT Expr))]),
--
--   ForallC [] [AppT (AppT EqualityT (VarT ix)) (ConT Expr)]
--     (NormalC ROSec [(NotStrict,AppT (ConT Ast) (ConT Expr)),(NotStrict,AppT (ConT Ast) (ConT Expr))]),
--
--   ForallC [] [AppT (AppT EqualityT (VarT ix)) (ConT Expr)]
--     (NormalC TyAnn [(NotStrict,AppT (ConT Ast) (ConT Expr)),(NotStrict,AppT (ConT Ast) (ConT Type))]),
--
--   ForallC [] [AppT (AppT EqualityT (VarT ix)) (ConT Expr)]
--     (NormalC If [(NotStrict,AppT (ConT Ast) (ConT Expr)),(NotStrict,AppT (ConT Ast) (ConT Expr)),(NotStrict,AppT (ConT Ast) (ConT Expr))]),
--
--   ForallC [] [AppT (AppT EqualityT (VarT ix)) (ConT Expr)]
--     (NormalC Lam [(NotStrict,AppT ListT (AppT (ConT Ast) (ConT Patt))),(NotStrict,AppT (ConT Ast) (ConT Expr))]),
--
--   ForallC [] [AppT (AppT EqualityT (VarT ix)) (ConT Expr)]
--     (NormalC Let [(NotStrict,AppT (ConT Ast) (ConT Decl)),(NotStrict,AppT (ConT Ast) (ConT Expr))]),
--
--   ForallC [] [AppT (AppT EqualityT (VarT ix)) (ConT Expr)]
--     (NormalC Alt [(NotStrict,AppT (ConT Ast) (ConT Patt)),(NotStrict,AppT (ConT Ast) (ConT Expr)),(NotStrict,AppT (ConT Ast) (ConT Decl))]),
--
--   ForallC [] [AppT (AppT EqualityT (VarT ix)) (ConT Expr)]
--     (NormalC Case [(NotStrict,AppT (ConT Ast) (ConT Expr)),(NotStrict,AppT ListT (AppT (ConT Ast) (ConT Expr)))])] [])
