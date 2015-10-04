{-# LANGUAGE TupleSections #-}

module Language.Indescript.TypeInference where

import Control.Monad.State
import Data.Monoid ((<>))
import Data.Map (Map, insert, (!))

import Language.Indescript.ADT

type Env = Map EVar Type

data Constraint  = Type :== Type
type Constraints = [Constraint]

nextTVar :: State Int Type
nextTVar = do count <- get
              put $ count + 1
              let tvar = LTVar $ "t" <> show count
              return $ TVar tvar

generateConstraints :: Env -> Exp -> State Int (Type, Constraints)
generateConstraints env (EVar v) = return (ty, [])
  where ty = env ! v
generateConstraints _   (ELit x) = return (ty, [])
  where ty = case x of LBool _ -> TBool
                       LInt  _ -> TInt
generateConstraints env (EApp f x) =
  do (t1, c1) <- generateConstraints env f
     (t2, c2) <- generateConstraints env x
     ty       <- nextTVar
     let cs   =  c1 <> c2 <> [t1 :== TArr t2 ty]
     return (ty, cs)
generateConstraints env (EAbs v e) =
  do t1      <- nextTVar
     (t2, c) <- generateConstraints (insert v t1 env) e
     return (TArr t1 t2, c)
generateConstraints env (EIf x l r) =
  do (tx, cx) <- generateConstraints env x
     (tl, cl) <- generateConstraints env l
     (tr, cr) <- generateConstraints env r
     let cs   =  cx <> cl <> cr <> [tx :== TBool, tl :== tr]
     return (tl, cs)
generateConstraints env (ELet f e1 e2) =
  do tf       <- nextTVar
     (t1, c1) <- generateConstraints (insert f tf env) e1
     (t2, c2) <- generateConstraints (insert f tf env) e2
     let cs   =  c1 <> c2 <> [tf :== t1]
     return (t2, cs)
