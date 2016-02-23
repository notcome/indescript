{-# OPTIONS_GHC -ddump-splices #-}

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module Language.Indescript.AST where

import Data.Proxy

import Control.IxPF
import Control.IxPF.TH
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
$(deriveIxPF     ''Ast)
-- $(deriveIxPFType ''Ast)
-- $(deriveIxPFTraversal ''Ast)
-- $(deriveIxPFToIxPF ''Ast)
