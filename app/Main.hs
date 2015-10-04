{-# LANGUAGE TupleSections, FlexibleContexts, TypeSynonymInstances, NoMonomorphismRestriction, OverlappingInstances, FlexibleInstances #-}

module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Functor ((<$>))
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import qualified Text.PrettyPrint as PP

data Exp     =  EVar EVar
             |  ELit Lit
             |  EApp Exp Exp
             |  EAbs EVar Exp
             |  ELet EVar Exp Exp
             deriving (Eq, Ord)

newtype EVar = EV String deriving (Eq, Ord)

data Lit     =  LInt Integer
             |  LBool Bool
             deriving (Eq, Ord)

data Type    =  TVar TVar
             |  TInt
             |  TBool
             |  Type `TArr` Type
             deriving (Eq, Ord)

newtype TVar = TV String deriving (Eq, Ord)
data Scheme  =  Forall [TVar] Type
newtype TypeEnv = TypeEnv (Map.Map EVar Scheme)


(\\) :: TypeEnv -> (EVar, Scheme) -> TypeEnv
(TypeEnv env) \\ (x, s) =  TypeEnv $ Map.insert x s env

type Subst = Map.Map TVar Type

class Substitutable a where
  apply     :: Subst -> a -> a
  freeTvars :: a -> Set.Set TVar



instance Substitutable Type where
  apply _  TInt            = TInt
  apply _  TBool           = TBool
  apply su t@(TVar a)      = Map.findWithDefault t a su
  apply su (t1 `TArr` t2)  = apply su t1 `TArr` apply su t2

  freeTvars TInt           =  Set.empty
  freeTvars TBool          =  Set.empty
  freeTvars (TVar a)       =  Set.singleton a
  freeTvars (t1 `TArr` t2) =  freeTvars t1 `Set.union` freeTvars t2


instance Substitutable Scheme where
  apply s (Forall as t)   = Forall as $ apply s' t
                            where s' = foldr Map.delete s as

  freeTvars (Forall as t) = (freeTvars t) `Set.difference` (Set.fromList as)



instance Substitutable a => Substitutable [a] where
  apply     = map . apply
  freeTvars = foldr Set.union Set.empty . map freeTvars


instance Substitutable TypeEnv where
  apply s   (TypeEnv env) =  TypeEnv   $ Map.map (apply s) env
  freeTvars (TypeEnv env) =  freeTvars $ Map.elems env



empSubst  ::  Subst
empSubst  =   Map.empty


after         :: Subst -> Subst -> Subst
su1 `after` su2 = (Map.map (apply su1) su2) `Map.union` su1


mgu (l `TArr` r) (l' `TArr` r')  = do  s1 <- mgu l l'
                                       s2 <- mgu (apply s1 r) (apply s1 r')
                                       return (s1 `after` s2)
mgu (TVar a) t                   = varAsgn a t
mgu t (TVar a)                   = varAsgn a t
mgu TInt TInt                    = return empSubst
mgu TBool TBool                  = return empSubst
mgu t1 t2                        = throwError $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2


varAsgn a t
  | t == TVar a                  =  return empSubst
  | a `Set.member` (freeTvars t) =  throwError $ "occur check fails: " ++ show a ++ " in " ++ show t
  | otherwise                    =  return $ Map.singleton a t


generalize        ::  TypeEnv -> Type -> Scheme
generalize env t  =   Forall as t
  where as = Set.toList $ (freeTvars t) `Set.difference` (freeTvars env)



data TIState = TIState { count :: Int }


fresh = do s     <- get
           let n = count s
           put   $ s { count = n + 1 }
           return n


freshTVar prefix = fresh >>= return . TVar . TV . (prefix ++) . show


instantiate (Forall as t) = do as' <- mapM (\ _ -> freshTVar "a") as
                               let s = Map.fromList $ zip as as'
                               return $ apply s t

ti ::  (MonadState TIState m, MonadError String m) =>
       TypeEnv -> Exp -> m (Subst, Type)



ti _ (ELit (LInt _))  = return (empSubst, TInt)
ti _ (ELit (LBool _)) = return (empSubst, TInt)


ti (TypeEnv env) (EVar x) =
    case Map.lookup x env of
       Nothing   ->  throwError $ "Unbound Variable: " ++ show x
       Just s    ->  instantiate s >>= return . \x -> (empSubst, x)--(empSubst,)


ti env (EAbs x e) =
    do  tv       <- freshTVar "a"
        let env' = env \\ (x, Forall [] tv)
        (s1, t1) <- ti env' e
        return (s1, (apply s1 tv) `TArr` t1)


ti env (EApp e1 e2) =
    do  tv       <- freshTVar "a"
        (s1, t1) <- ti env e1
        (s2, t2) <- ti (apply s1 env) e2
        s3       <- mgu (apply s2 t1) (TArr t2 tv)
        return (s3 `after` s2 `after` s1, apply s3 tv)


ti env (ELet x e1 e2) =
    do  (s1, t1) <- ti env e1
        let env'  = apply s1 env
            t'    = generalize env' t1
        (s2, t2) <- ti (env' \\ (x, t')) e2
        return (s1 `after` s2, t2)


-- type TI a = ErrorT String (State TIState) a

typeInference env e = uncurry apply <$> res
  where act = ti env e
        res = evalState (runErrorT act) s0
        s0  = TIState { count = 0 }




e0  =  ELet (EV "id") (EAbs (EV "x") (EVar (EV "x")))
        (EVar (EV "id"))

e1  =  ELet (EV "id") (EAbs (EV "x") (EVar (EV "x")))
        (EApp (EVar (EV "id")) (EVar (EV "id")))

e2  =  ELet (EV "id") (EAbs (EV "x") (ELet (EV "y") (EVar (EV "x")) (EVar (EV "y"))))
        (EApp (EVar (EV "id")) (EVar (EV "id")))

e3  =  ELet (EV "id") (EAbs (EV "x") (ELet (EV "y") (EVar (EV "x")) (EVar (EV "y"))))
        (EApp (EApp (EVar (EV "id")) (EVar (EV "id"))) (ELit (LInt 2)))

e4  =  ELet (EV "id") (EAbs (EV "x") (EApp (EVar (EV "x")) (EVar (EV "x"))))
        (EVar (EV "id"))

e5  =  EAbs (EV "m") (ELet (EV "y") (EVar (EV "m"))
                     (ELet (EV "x") (EApp (EVar (EV "y")) (ELit (LBool True)))
                          (EVar (EV "x"))))

test :: Exp -> IO ()
test e = case typeInference (TypeEnv Map.empty) e of
           Left err  ->  putStrLn $ "error: " ++ err
           Right t   ->  putStrLn $ show e ++ " :: " ++ show t


main :: IO ()
main = mapM_ test [e0, e1, e2, e3, e4, e5]






instance Show TVar where
  showsPrec _ x = shows (prTVar x)

prTVar (TV a) = PP.text a

instance Show Type where
  showsPrec _ x = shows (prType x)

prType             ::  Type -> PP.Doc
prType (TVar a)    =   prTVar a
prType TInt        =   PP.text "Int"
prType TBool       =   PP.text "Bool"
prType (TArr t s)  =   prParenType t PP.<+> PP.text "->" PP.<+> prType s

prParenType     ::  Type -> PP.Doc
prParenType  t  =   case t of
                      TArr _ _  -> PP.parens (prType t)
                      _         -> prType t

instance Show EVar where
  showsPrec _ x = shows (prEVar x)

instance Show Exp where
  showsPrec _ x = shows (prExp x)

prEVar (EV x)          = PP.text x

prExp                  ::  Exp -> PP.Doc
prExp (EVar x)         =   prEVar x
prExp (ELit lit)       =   prLit lit
prExp (ELet x b body)  =   PP.text "let" PP.<+>
                           prEVar x PP.<+> PP.text "=" PP.<+>
                           prExp b PP.<+> PP.text "in" PP.$$
                           PP.nest 2 (prExp body)
prExp (EApp e1 e2)     =   prExp e1 PP.<+> prParenExp e2
prExp (EAbs x e)       =   PP.char '\\' PP.<+> prEVar x PP.<+>
                           PP.text "->" PP.<+>
                           prExp e


prParenExp    ::  Exp -> PP.Doc
prParenExp t  =   case t of
                    ELet _ _ _  -> PP.parens (prExp t)
                    EApp _ _    -> PP.parens (prExp t)
                    EAbs _ _    -> PP.parens (prExp t)
                    _           -> prExp t

instance Show Lit where
    showsPrec _ x = shows (prLit x)

prLit            ::  Lit -> PP.Doc
prLit (LInt i)   =   PP.integer i
prLit (LBool b)  =   if b then PP.text "True" else PP.text "False"

instance Show Scheme where
    showsPrec _ x = shows (prScheme x)

prScheme                ::  Scheme -> PP.Doc
prScheme (Forall as t)  =   PP.text "All" PP.<+>
                            PP.hcat (PP.punctuate PP.comma (map prTVar as))
                            PP.<> PP.text "." PP.<+> prType t
