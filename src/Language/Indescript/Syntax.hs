{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds      #-}

module Language.Indescript.Syntax where

newtype IxFix f i = In { out :: f (IxFix f) i }

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

type AnnotAstF a f i = (a, AstF f i)

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
  BindF  :: [f AstPatt] -> f AstPatt -> AstF f AstPatt
  HoleF  :: AstF f AstPatt
  EqtF   :: FnLhs f -> f AstExpr -> [f AstDecl] -> AstF f AstDecl
  EqtsF  :: [f AstDecl] -> AstF f AstDecl
  OpFixF :: AssocType -> Int -> [f AstExpr] -> AstF f AstDecl
  TySigF :: [f AstExpr] -> f AstType -> AstF f AstDecl
  TyAlsF :: f AstType -> f AstType -> AstF f AstDecl
  NewTyF :: f AstType -> f AstType -> f AstType -> AstF f AstDecl
  DatTyF :: f AstType -> [f AstType] -> AstF f AstDecl

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

data FnLhs (f :: AstIx -> *) = FnArgs [f AstExpr] (f AstPatt)
                                | FnOp   (f AstPatt) (f AstExpr) (f AstPatt)

data AssocType = Infix | InfixL | InfixR deriving (Eq, Show)
