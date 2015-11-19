{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TupleSections  #-}

module Language.Indescript.Syntax where

import Control.IxFix

-- ####
--    # Type Indexes and Proxies
data AstIx = AstDecl | AstExpr | AstPatt | AstType

--   ## Type Synonyms for Promoted Term Constrcutors
type AstDecl = 'AstDecl
type AstExpr = 'AstExpr
type AstPatt = 'AstPatt
type AstType = 'AstType

--   ## Proxies
data AstIxProxy (i :: AstIx) where
  DeclProxy :: AstIxProxy AstDecl
  ExprProxy :: AstIxProxy AstExpr
  PattProxy :: AstIxProxy AstPatt
  TypeProxy :: AstIxProxy AstType

--    # AST Base Functor
data AstF (f :: AstIx -> *) (i :: AstIx) where
--   ## Atomic AST Nodes (Expr, Patt, Decl)
  VarF   :: Var -> AstIxProxy i -> AstF f i
  ConF   :: Var -> AstIxProxy i -> AstF f i
  LitF   :: Lit -> AstIxProxy i -> AstF f i
  ParenF :: f i -> AstF f i
--   ## Complex AST Expression
  AppF   :: f i -> [f i] -> AstF f i
  InfixF :: f i -> f i -> f i -> AstF f i
  NegF   :: f AstExpr -> AstF f AstExpr
  LOSecF :: f AstExpr -> f AstExpr -> AstF f AstExpr
  ROSecF :: f AstExpr -> f AstExpr -> AstF f AstExpr
  TyAnnF :: f AstExpr -> f AstType -> AstF f AstExpr
  IfF    :: f AstExpr -> f AstExpr -> f AstExpr -> AstF f AstExpr
  LamF   :: [f AstPatt] -> f AstExpr -> AstF f AstExpr
  LetF   :: [f AstDecl] -> f AstExpr -> AstF f AstExpr
  AltF   :: f AstPatt -> f AstExpr -> [f AstDecl] -> AstF f AstExpr
  CaseF  :: f AstExpr -> [f AstExpr] -> AstF f AstExpr
--   ## Complex AST Type
-- Generalized types, also known as “schemes”.
  TyGenF :: [f AstType] -> f AstType -> AstF f AstType
--   ## Complex AST Pattern
  BindF  :: f AstPatt -> f AstPatt -> AstF f AstPatt
  HoleF  :: AstF f AstPatt
--   ## Complex AST Declarations
-- Ordinary equation/function definitions.
  EqtF   :: FnLhsF f AstExpr -> f AstExpr -> [f AstDecl] -> AstF f AstDecl
-- Equation groups.
-- Used both for multiple branch of a single function and binding groups during
--   type inference in order to obtain most generalized types.
  EqtsF  :: [f AstDecl] -> AstF f AstDecl
-- Operator fixity declarations.
-- Removed after adjusting AST.
  OpFixF :: AssocType -> Int -> [f AstExpr] -> AstF f AstDecl
  TySigF :: [f AstExpr] -> f AstType -> AstF f AstDecl
  TyAlsF :: f AstType -> f AstType -> AstF f AstDecl
  NewTyF :: f AstType -> f AstType -> f AstType -> AstF f AstDecl
  DatTyF :: f AstType -> [f AstType] -> AstF f AstDecl

instance IxFunctor AstF where imap = imapDefault

-- TODO: use Template Haskell to automatically derivie the following instance.
instance IxTraversable AstF where
  itraverse f = \case
    (VarF var i)   -> VarF <$> pure var <*> pure i
    (ConF con i)   -> ConF <$> pure con <*> pure i
    (LitF lit i)   -> LitF <$> pure lit <*> pure i
    (ParenF e)     -> ParenF <$> f e
    (AppF fn args) -> AppF <$> f fn <*> traverse f args
    (InfixF l o r) -> InfixF <$> f l <*> f o <*> f r
    (NegF e)       -> NegF <$> f e
    (LOSecF o x)   -> LOSecF <$> f o <*> f x
    (ROSecF o x)   -> LOSecF <$> f o <*> f x
    (TyAnnF e t)   -> TyAnnF <$> f e <*> f t
    (IfF e1 e2 e3) -> IfF <$> f e1 <*> f e2 <*> f e3
    (LamF patts e) -> LamF <$> traverse f patts <*> f e
    (LetF decls e) -> LetF <$> traverse f decls <*> f e
    (AltF p e ds)  -> AltF <$> f p <*> f e <*> traverse f ds
    (CaseF e alts) -> CaseF <$> f e <*> traverse f alts
    (TyGenF ts ty) -> TyGenF <$> traverse f ts <*> f ty
    (BindF as src) -> BindF <$> f as <*> f src
    (HoleF)        -> pure HoleF
-- Declarations
    (EqtF lhs e whs) -> EqtF <$> itraverse f lhs <*> f e <*> traverse f whs
    (EqtsF eqts)     -> EqtsF <$> traverse f eqts
    (OpFixF d l ops) -> OpFixF d l <$> traverse f ops
    (TySigF xs ty)   -> TySigF <$> traverse f xs <*> f ty
    (TyAlsF t' t)    -> TyAlsF <$> f t' <*> f t
    (NewTyF t' c t)  -> NewTyF <$> f t' <*> f c <*> f t
    (DatTyF t cs)    -> DatTyF <$> f t <*> traverse f cs

newtype AnnotAstF a f i = Annot { unAnnot :: (a, AstF f i) }

instance IxFunctor (AnnotAstF a) where imap = imapDefault

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

data AssocType = Infix | InfixL | InfixR deriving (Eq, Show)

data FnLhsF (f :: AstIx -> *) (i :: AstIx) where
  FnArgsF :: f AstExpr -> [f AstExpr] -> FnLhsF f AstExpr
  FnOpF   :: f AstExpr -> f AstExpr -> f AstExpr -> FnLhsF f AstExpr

instance IxFunctor FnLhsF where imap = imapDefault

-- TODO: use Template Haskell to automatically derivie the following instance.
instance IxTraversable FnLhsF where
  itraverse f = \case
    FnArgsF fn xs -> FnArgsF <$> f fn <*> traverse f xs
    FnOpF   l o r -> FnOpF <$> f l <*> f o <*> f r

removeAnnot :: IxFix (AnnotAstF a) i -> IxFix AstF i
removeAnnot = cata (In . snd . unAnnot)

{-
collectBoundNames :: IxFix (AnnotAstF ElemPos) i
                  -> IxFix (AnnotAstF _) i
collectBoundNames = cata (In . collect . unAnnot) where
  collect (pos, ast) = (\case
    e@(LamF patts _) -> let bns   = map boundNames patts
                            annt' = SomeCon bns pos
                        in Annot (annt', e)
    e@(LetF decls _) -> let bns   = map boundNames decls
                            annt' = SomeCon bns pos
                        in Annot (annt', e)
    ) ast
-}
