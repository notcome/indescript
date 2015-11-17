-- Thanks /u/AndrasKovacs for pointing out the index type families solution
-- https://www.reddit.com/r/haskell/comments/3sm1j1/how_to_mix_the_base_functorrecursion_scheme_stuff/cwyr61h
-- and his standalone example:
-- https://gist.github.com/AndrasKovacs/af856be6cf816e08da95

-- The code will be refactored soon.

[/u/AndrasKovacs](

{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TupleSections     #-}

module Language.Indescript.Syntax where

import Data.Traversable

infixr 5 ~>
type f ~> g = forall i. f i -> g i

newtype K a b = K { unK :: a }
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype I a = I { unI :: a }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative I where
  pure = I
  I f <*> I a = I (f a)

instance Monoid a => Applicative (K a) where
  pure _ = K mempty
  K a <*> K a' = K (mappend a a')

class IxFunctor (f :: (k -> *) -> (k -> *)) where
  imap :: (a ~> b) -> (f a ~> f b)

class IxFunctor t => IxTraversable t where
  itraverse :: Applicative f
            => (forall i. a i -> f (b i))
            -> (forall i. t a i -> f (t b i))

imapDefault :: IxTraversable t => (a ~> b) -> (t a ~> t b)
imapDefault f = unI . itraverse (I . f)

newtype IxFix f i = In { out :: f (IxFix f) i }

cata :: IxFunctor f => (f a ~> a) -> (IxFix f ~> a)
cata phi = phi . imap (cata phi) . out

data AstIx = AstDecl | AstExpr | AstPatt | AstType

type AstDecl = 'AstDecl
type AstExpr = 'AstExpr
type AstPatt = 'AstPatt
type AstType = 'AstType

data AstIxProxy (i :: AstIx) where
  DeclProxy :: AstIxProxy AstDecl
  ExprProxy :: AstIxProxy AstExpr
  PattProxy :: AstIxProxy AstPatt
  TypeProxy :: AstIxProxy AstType

newtype AnnotAstF a f i = Annot { unAnnot :: (a, AstF f i) }

data AstF (f :: AstIx -> *) (i :: AstIx) where
  VarF   :: Var -> AstIxProxy i -> AstF f i
  LitF   :: Lit -> AstF f AstExpr
  ParenF :: f i -> AstF f i
  AppF   :: f i -> [f i] -> AstF f i
  InfixF :: f i -> f i -> f i -> AstF f i
  NegF   :: f AstExpr -> AstF f AstExpr
  LOSecF :: f AstExpr -> f AstExpr -> AstF f AstExpr
  ROSecF :: f AstExpr -> f AstExpr -> AstF f AstExpr
  TyAnnF :: f AstExpr -> f AstType -> AstF f AstExpr
  IfF    :: f AstExpr -> f AstExpr -> f AstExpr -> AstF f AstExpr
  LamF   :: [f AstPatt] -> f AstExpr -> AstF f AstExpr
  LetF   :: [f AstDecl] -> f AstExpr -> AstF f AstExpr
  CaseF  :: f AstExpr -> [Altn f] -> AstF f AstExpr
  TyGenF :: [f AstType] -> f AstType -> AstF f AstType
  BindF  :: f AstPatt -> f AstPatt -> AstF f AstPatt
  HoleF  :: AstF f AstPatt
  EqtF   :: FnLhs f -> f AstExpr -> [f AstDecl] -> AstF f AstDecl
  EqtsF  :: [f AstDecl] -> AstF f AstDecl
  OpFixF :: AssocType -> Int -> [f AstExpr] -> AstF f AstDecl
  TySigF :: [f AstExpr] -> f AstType -> AstF f AstDecl
  TyAlsF :: f AstType -> f AstType -> AstF f AstDecl
  NewTyF :: f AstType -> f AstType -> f AstType -> AstF f AstDecl
  DatTyF :: f AstType -> [f AstType] -> AstF f AstDecl

instance IxFunctor AstF where
  imap = imapDefault

instance IxFunctor (AnnotAstF a) where
  imap = imapDefault

instance IxTraversable AstF where
  itraverse f = \case
    (VarF var i)   -> VarF <$> pure var <*> pure i
    (LitF lit)     -> LitF <$> pure lit
    (ParenF e)     -> ParenF <$> f e
    (AppF fn args) -> AppF <$> f fn <*> mapList f args
    (InfixF l o r) -> InfixF <$> f l <*> f o <*> f r
    (NegF e)       -> NegF <$> f e
    (LOSecF o x)   -> LOSecF <$> f o <*> f x
    (ROSecF o x)   -> LOSecF <$> f o <*> f x
    (TyAnnF e t)   -> TyAnnF <$> f e <*> f t
    (IfF e1 e2 e3) -> IfF <$> f e1 <*> f e2 <*> f e3
    (LamF patts e) -> LamF <$> mapList f patts <*> f e
    (LetF decls e) -> LetF <$> mapList f decls <*> f e
    (CaseF e alts) -> CaseF <$> f e <*> alts'
      where alts'        = mapList f' alts
            f' (a, b, c) = (,,) <$> f a <*> f b <*> mapList f c
    (TyGenF ts ty) -> TyGenF <$> mapList f ts <*> f ty
    (BindF as src) -> BindF <$> f as <*> f src
    (HoleF)        -> pure HoleF

    (EqtF lhs e whs) -> let
      lhs' = case lhs of
        FnArgs fn xs -> FnArgs <$> f fn <*> mapList f xs
        FnOp l o r   -> FnOp <$> f l <*> f o <*> f r
      in EqtF <$> lhs' <*> f e <*> mapList f whs
    (EqtsF eqts)     -> EqtsF <$> mapList f eqts
    (OpFixF d l ops) -> OpFixF d l <$> mapList f ops
    (TySigF xs ty)   -> TySigF <$> mapList f xs <*> f ty
    (TyAlsF t' t)    -> TyAlsF <$> f t' <*> f t
    (NewTyF t' c t)  -> NewTyF <$> f t' <*> f c <*> f t
    (DatTyF t cs)    -> DatTyF <$> f t <*> mapList f cs
    where mapList f xs = sequenceA $ map f xs

instance IxTraversable (AnnotAstF x) where
  itraverse f (Annot (x, t)) = (Annot . (x,)) <$> itraverse f t

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

type Altn  (f :: AstIx -> *) = (f AstPatt, f AstExpr, [f AstDecl])

data FnLhs (f :: AstIx -> *) = FnArgs (f AstExpr) [f AstPatt]
                             | FnOp   (f AstPatt) (f AstExpr) (f AstPatt)

data AssocType = Infix | InfixL | InfixR deriving (Eq, Show)

removeAnnot :: IxFix (AnnotAstF a) i -> IxFix AstF i
removeAnnot = cata (In . snd . unAnnot)
