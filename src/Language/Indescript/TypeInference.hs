{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}

module Language.Indescript.TypeInference where

import Control.Monad.State
import Data.Monoid ((<>))
import qualified Data.Map.Strict as Map

import Language.Indescript.ADT

type Env = Map.Map EVar Type
prelude :: Env
prelude = Map.fromList [
    (LEVar "eq", TArr (TVar $ LTVar "a") (TArr (TVar $ LTVar "a") TBool))
  , (LEVar "add", TArr TInt (TArr TInt TInt))
  , (LEVar "sub", TArr TInt (TArr TInt TInt))]

data Constraint  = Type :== Type deriving Show
type Constraints = [Constraint]

nextTVar :: State Int Type
nextTVar = do count <- get
              put $ count + 1
              let tvar = LTVar $ "t" <> show count
              return $ TVar tvar

generateConstraints :: Env -> Exp -> State Int (Type, Constraints)
generateConstraints env (EVar v) = return (ty, [])
  where ty = case Map.lookup v env of
          (Just x) -> x
          Nothing  -> error $ show v <> " not found"
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

type Substitution = Map.Map TVar Type

(|=>) :: TVar -> Type -> Substitution
v |=> t = Map.singleton v t

after :: Substitution -> Substitution -> Substitution
σ2 `after` σ1 = Map.union σ2' σ2
  where σ2' = Map.map (apply σ2) σ1

class Substitutable a where
  apply    :: Substitution -> a -> a

instance Substitutable Type where
  apply _ TBool        = TBool
  apply _ TInt         = TInt
  apply σ t@(TVar v)   = Map.findWithDefault t v σ
  apply σ (TArr t1 t2) = apply σ t1 `TArr` apply σ t2

instance Substitutable Constraint where
  apply σ (t1 :== t2) = apply σ t1 :== apply σ t2

instance Substitutable Constraints where
  apply σ = fmap $ apply σ

unify :: Constraints -> Substitution
unify []              = Map.empty
unify ((s :== t):cs') = algorithm s t cs'
  where
    isTVar (TVar _) = True
    isTVar _        = False

    trySubst :: TVar -> TVar -> Constraint -> [TVar]
    trySubst s t ((TVar l) :== (TVar r)) = if pred l r then [r]
                                      else if pred r l then [l]
                                      else []
      where eqST x   = x == s || x == t
            pred l r = eqST l && (not . eqST) r
    trySubst _ _ _                       = []

    algorithm :: Type -> Type -> Constraints -> Substitution
    algorithm s t cs
      | s == t          = unify cs
      | not $ isTVar s
      , isTVar t        = unify $ (t :== s):cs
      | TArr s1 s2 <- s
      , TArr t1 t2 <- t = unify $ [s1 :== t1, s2 :== t2] <> cs
      | TVar s' <- s
      , TVar t' <- t    = let
        substs  = Map.fromList $ map (, t) $ concatMap (trySubst s' t') cs
        substs' = (s' |=> t) `after` substs
        in (unify $ apply substs' cs) `after` substs'
      | TVar s' <- s    = (unify $ apply (s' |=> t) cs) `after` (s' |=> t)
      | otherwise       = error "unification failed"

infer :: Exp -> Type
infer exp = let (ty, cs)     = evalState (generateConstraints prelude exp) 0
                substitution = unify cs
            in apply substitution ty
