{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts    #-}

module Language.Indescript.TypeInference where

import Control.Monad.State
import Control.Monad.Except
import Data.Monoid ((<>))
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.List       as List

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
     (t2, c) <- generateConstraints (Map.insert v t1 env) e
     return (TArr t1 t2, c)
generateConstraints env (EIf x l r) =
  do (tx, cx) <- generateConstraints env x
     (tl, cl) <- generateConstraints env l
     (tr, cr) <- generateConstraints env r
     let cs   =  cx <> cl <> cr <> [tx :== TBool, tl :== tr]
     return (tl, cs)
generateConstraints env (ELet f e1 e2) =
  do tf       <- nextTVar
     (t1, c1) <- generateConstraints (Map.insert f tf env) e1
     (t2, c2) <- generateConstraints (Map.insert f tf env) e2
     let cs   =  c1 <> c2 <> [tf :== t1]
     return (t2, cs)

type Substitution = Map TVar Type

(|=>) :: TVar -> Type -> Substitution
v |=> t = Map.singleton v t

after :: Substitution -> Substitution -> Substitution
after = undefined

class Substitutable a where
  apply    :: Substitution -> a -> a
  freeVars :: a -> [TVar]

instance Substitutable Constraints where
  apply    = undefined
  freeVars = undefined

unify :: Constraints -> Substitution
unify []              = Map.empty
unify ((s :== t):cs') = case runExcept proc of
    (Left  subst) -> subst
    (Right msg)   -> error msg
  where
    sEqX (s' :== (TVar _)) = s' == s
    sEqX _                 = False
    xEqT ((TVar _) :== t') = t' == t
    xEqT _                 = False

    toArrow (TArr s1 s2) (TArr t1 t2) = Just (s1, s2, t1, t2)
    toArrow _            _            = Nothing

    tryBool :: (MonadError e m) => Bool -> e -> m ()
    tryBool cond res = if cond then throwError res
                   else return ()
    tryMaybe :: (MonadError e m) => Maybe a -> (a -> e) -> m ()
    tryMaybe (Just x) f = throwError $ f x
    tryMaybe Nothing  _ = return ()
    proc = do
      tryBool (s == t) (unify cs')
      tryMaybe (List.find sEqX cs') (\(_ :== (TVar x))
        -> (unify $ apply (x |=> t) cs') `after` (x |=> t))
      tryMaybe (List.find xEqT cs') (\((TVar x) :== _)
        -> (unify $ apply (x |=> s) cs') `after` (x |=> s))
      tryMaybe (toArrow s t) (\(s1, s2, t1, t2)
        -> unify $ cs' <> [s1 :== t1, s2 :== t2])
      return "unification failed"
