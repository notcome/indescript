{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind     #-}

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.Indescript.Parser where

import Control.Applicative
import Control.Monad.State (evalState)

import qualified Text.Megaparsec as MP

import Language.Indescript.Syntax
import Language.Indescript.Parser.Prim
import Language.Indescript.Parser.Pos
import Language.Indescript.Parser.Lexer

import Debug.Trace

--    # Primitives
--   ## Special Characters
--  ### Literal Values
lparen = reserved "("
rparen = reserved ")"
lsquar = reserved "["
rsquar = reserved "]"
lbrace = reserved "{"
rbrace = reserved "}"

backtick  = reserved "`"
semicolon = reserved ";"
comma     = reserved ","

negsign = token $ TkVar $ VarSym "-"
dotsign = token $ TkVar $ VarSym "."

equal  = reserved "="
sarrow = reserved "->"
darrow = reserved "=>"

--  ### Combinators
paren p = lparen *> p <* rparen
brace p = lbrace *> p <* rbrace

-- { p1; p2; ... }
-- TODO: wait until the next release of megaparsec.
braceBlock p = brace $ sepEndBy p semicolon
  where
    sepEndBy p sep = sepEndBy1 p sep <|> pure []
    sepEndBy1 p sep = (:) <$> p <*> ((sep *> sepEndBy p sep) <|> pure [])

--   ## Simple Combinators
--  ### Extractors
varid, conid, varsym, consym :: ISParser s m => m Variable
varid = satisfy test >>= extractTkVar
  where test (TkVar (VarId _))   = True
        test _                   = False

conid = satisfy test >>= extractTkVar
  where test (TkVar (ConId _))   = True
        test _                   = False

varsym = satisfy test >>= extractTkVar
  where test (TkVar (VarSym _))  = True
        test _                   = False

consym = satisfy test >>= extractTkVar
  where test (TkVar (ConSym _))  = True
        test _                   = False

extractTkVar ((TkVar x), _) = return x
extractTkVar _              = impossible

literal :: ISParser s m => m Literal
literal = satisfy test >>= extract
  where test (TkLit _) = True
        test _         = False
        extract (TkLit x, _) = return x
        extract _            = impossible

--  ### Combinators
-- #### Concrete Combinators
litCon, cons, var, con :: ISParser s m => m Variable
litCon =  MP.try (paren pTupleCon)
      <|> MP.try (paren $ return (ConSym "()"))
      <|> lsquar *> rsquar *> return (ConSym "[]")
  where
    pTupleCon = MP.some comma >>= (return . mkTuple . length)
-- TODO: move mkTuple to a more general position.
    mkTuple n = ConSym $ "(" ++ replicate n ',' ++ ")"

cons = reserved ":" *> return (ConSym ":")
var  = varid <|> MP.try (paren varsym)
con  = conid <|> MP.try (paren (consym <|> cons)) <|> litCon

pOp, pConOp, pTyConOp :: ISParser s m => m (Op ElemPos)
pOp = scope $ (oid <|> sym) >>= return . Op
  where oid = backtick *> (varid <|> conid) <* backtick
        sym = varsym <|> consym

pConOp = scope $ (oid <|> sym <|> cons) >>= return . Op
  where oid = backtick *> conid <* backtick
        sym = varsym <|> consym

pTyConOp = pConOp <|> arrowOp
  where arrowOp = scope $ sarrow *> return (Op $ ConSym "->")

pWhere :: ISParser s m => m [Decl ElemPos]
pWhere = toList <$> optional (reserved "where" *> braceBlock pDecl)
  where
    toList Nothing   = []
    toList (Just xs) = xs

-- #### Abstract Combinators
pFXs :: (ISParser s m
        , a ElemPos ~ pa, b ElemPos ~ pb
        , GetElemPos pa, GetElemPos pb)
     => m pa -> m pb -> (pa -> [pb] -> ElemPos -> pa) -> m pa
pFXs pf px comb = liftA2 comb' pf (MP.many px)
  where comb' f [] = f
        comb' f xs = comb f xs $ elemPos (f, xs)

--    # Non-trivial Combinators
--   ## Type
pType, pFType, pAType :: ISParser s m => m (Type ElemPos)
pType = pForall <|> pInfix
  where
    pForall = do env <- reserved "forall" *> MP.many pTyVar <* dotsign
                 ty  <- pInfix
                 return $ TForall env ty $ elemPos (env, ty)

    pInfix = MP.try pOpType <|> pFType
      where pOpType = scope $ liftA3 TInfix pFType pTyConOp pType

pFType = pFXs pAType pAType TApp

pAType = pCon <|> pTyVar <|> pParened
  where
    pCon = scope $ fmap TCon con'
      where con'    = con <|> MP.try pSArrow
            pSArrow = paren sarrow *> return (ConSym "->")
    pParened = scope $ paren pType >>= (return . flip updateAST)

pTyVar :: ISParser s m => m (Type ElemPos)
pTyVar = scope $ fmap TVar varid

--   ## Pattern
pPat, pLPat, pFPat, pAPat :: ISParser s m => m (Pat ElemPos)
pPat = MP.try pOpCon <|> pLPat
  where
    pOpCon = scope $ liftA3 PInfix pLPat pConOp pPat

pLPat = pFPat <|> pAPat <|> pNeg
  where
    pNeg = scope $ do _   <- negsign
                      lit <- literal
                      case lit of
                        LInt   n -> return $ PLit $ LInt   (-n)
                        LFloat n -> return $ PLit $ LFloat (-n)
                        _        -> fail "expected a number."

pFPat = pFXs (scope $ fmap PCon con) pAPat PApp

-- TODO: add support for tuple, list, labeled pattern, and irrefutable pattern
pAPat = MP.try pAs' <|> pAs <|> pCon <|> pLit <|> pWildcard <|> pParened
  where
    pAs  = scope $ fmap PVar var
    pAs' = scope $ liftA2 PAs pAs (reserved "@" *> pAPat)
    pCon = scope $ fmap PCon con
    pLit = scope $ fmap PLit literal
    pWildcard = scope $ reserved "_" *> return PWildcard
    pParened  = scope $ paren pPat >>= return . flip updateAST

--   ## Expression
pExpr, pLExpr, pFExpr, pAExpr :: ISParser s m => m (Expr ElemPos)
-- TODO: add support for type ascription, exp :: type
pExpr = MP.try pOpExpr <|> pNeg <|> pLExpr
  where
    pOpExpr = scope $ liftA3 EInfix pLExpr (pOp <|> pConOp) pExpr
    pNeg = scope $ fmap ENeg (negsign *> pExpr)

-- TODO: add support for do-notation [after typeclass]
pLExpr = pLExpr' <|> pFExpr
  where
    pLExpr' = scope $ pLam <|> pLet <|> pIf <|> pCase {-<|> pDo-}

    pLam = liftA2 ELam (reserved "\\"   *> MP.some pAPat)
                       (sarrow          *> pExpr)
    pLet = liftA2 ELet (reserved "let"  *> braceBlock pDecl)
                       (reserved "in"   *> pExpr)
-- TODO: add support for optionl semicolon.
    pIf  = liftA3 EIf  (reserved "if"   *> pExpr)
                       (reserved "then" *> pExpr)
                       (reserved "else" *> pExpr)
    pCase = liftA2 ECase (reserved "case" *> pExpr)
                         (reserved "of"   *> braceBlock pAlt)
      where pAlt = scope $ liftA3 EAlt pPat (sarrow *> pExpr) pWhere

pFExpr = pFXs pAExpr pAExpr EApp

-- TODO: add support for tuple, list, labeled construction and update, and
--       consider whether to add arithmetic seqeunce and list comprehension
pAExpr = scope $  pVar <|> pCon <|> pLit
              <|> pParened <|> pLOpSec <|> pROpSec
  where
    pVar = fmap EVar var
    pCon = fmap ECon con
    pLit = fmap ELit literal
    pParened = fmap EParen $ paren pExpr
    pLOpSec = paren $ liftA2 (flip ELOpSec) pExpr pOp
    pROpSec = paren parser
      where parser = do op@(Op opv _) <- pOp
                        x <- pExpr
                        case opv of
                          VarSym "-" -> return $ ENeg x
                          _          -> return $ EROpSec op x

--    # Declaration
--   ## Type, Newtype, Data
-- TODO: add support for type class.

pTypeAlias, pNewType, pDataType :: ISParser s m => m (Decl ElemPos)
pTypeAlias = scope $ liftA2 DeclTypeAlias pLhs pType
  where
    pLhs = reserved "type" *> pSimpleType <* equal

-- TODO: add support for the field-like syntax.
pNewType = scope $ liftA3 DeclNewType pLhs pTCon pAType
  where
    pLhs  = reserved "newtype" *> pSimpleType <* equal
    pTCon = scope $ fmap TCon conid

pDataType = scope $ liftA2 DeclDataType pLhs pRhs
  where
    pLhs = reserved "data" *> pSimpleType
    pRhs = equal *> (MP.sepBy1 pConstr $ reserved "|")
      where
        pConstr  = MP.try pInfix <|> pLConstr
        pInfix   = scope $ liftA3 TInfix pFType pConOp pFType
        pLConstr = pFXs pCon pAType TApp
        pCon     = scope $ fmap TCon con

pSimpleType :: ISParser s m => m (Type ElemPos)
pSimpleType = pPrefix <|> pInfix
  where pPrefix = pFXs pSTCon pTyVar TApp
        pInfix = scope $ liftA3 TInfix pTyVar pConOp pTyVar
        pSTCon = scope $ fmap TCon (conid <|> MP.try (paren consym))

--   ## Function Declaration
pGenDecl :: ISParser s m => m (Decl ElemPos)
pGenDecl = scope $ pTypeSig <|> pFixity
  where
-- TODO: add support for type signature with context.
    pTypeSig = liftA2 DeclTypeSig pVars (reserved "::" *> pType)
      where pVars = MP.sepBy1 pVar comma
            pVar  = scope $ fmap Var (var <|> con)
    pFixity  = fmap DeclFixity pFixity'
      where
        pFixity' = scope $ liftA3 mkFixity pAssocType (MP.optional pInt) pOps
        mkFixity at Nothing  ops = Fixity at ops
        mkFixity at (Just l) ops = FixityLv at l ops

        pAssocType =  reserved "infixl" *> return InfixL
                  <|> reserved "infixr" *> return InfixR
                  <|> reserved "infix"  *> return Infix

        pInt :: ISParser s m => m Int
        pInt = satisfy test >>= extract
          where test (TkLit _) = True
                test _         = False
                extract (TkLit (LInt x), _) = return x
                extract _                   = impossible

        pOps = MP.sepBy1 pOp comma

pDecl :: ISParser s m => m (Decl ElemPos)
pDecl =  MP.try pGenDecl <|> pDecl'
  where
    pDecl' = scope $ liftA3 DeclFn pLhs (equal *> pRhs) pWhere
    pLhs = MP.try pOpLhs <|> pLLhs
      where
        pOpLhs = scope $ liftA3 FnOp pPat pOp pPat
        pLLhs  = p1 <|> p2
          where
            p1 = scope $ liftA2 FnArgs var' (MP.many pAPat)
              where var' = scope $ fmap Var var
            p2 = scope $ do (FnArgs f xs _) <- paren p1
                            rest            <- MP.some pAPat
                            return $ FnArgs f (xs ++ rest)

-- TODO: add support for guard expression.
    pRhs = pExpr

pTopDecl :: ISParser s m => m (Decl ElemPos)
pTopDecl = pTypeAlias <|> pNewType <|> pDataType <|> pDecl
--   ## Module
pModule :: ISParser s m => m [Decl ElemPos]
pModule = reserved "module" *> reserved "where" *> braceBlock pTopDecl

--    # Interface
parse' parser srcName input = let
  (Just tokens)  = lexIndescript input
  stateMonad     = MP.runParserT parser srcName tokens
  (Right result) = evalState stateMonad $ ElemPos pesudoPoint zeroSpan
  in result

testParse parser input = let
  parser'        = parser <* MP.eof
  (Just tokens)  = lexIndescript input
  stateMonad     = MP.runParserT parser' "(test)" tokens
  in evalState stateMonad $ ElemPos pesudoPoint zeroSpan

parse :: String -> String -> [Decl ElemPos]
parse = parse' (pModule <* MP.eof)
