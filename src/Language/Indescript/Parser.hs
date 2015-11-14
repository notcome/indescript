{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind     #-}

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase                #-}
module Language.Indescript.Parser where

import Control.Applicative
import Control.Monad.State (evalState)

import qualified Text.Megaparsec as MP

import Language.Indescript.Syntax
import Language.Indescript.Parser.Prim
import Language.Indescript.Parser.Pos
import Language.Indescript.Parser.Lexer

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
braceBlock p = brace $ sepEndBy p semicolon

--   ## Simple Combinators
--  ### Extractors
varid, conid, varsym, consym :: ISParser s m => m Var
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

conCons, conArrow :: ISParser s m => m Var
conCons = reserved ":" *> return (ConSym ":")
conArrow = sarrow *> return (ConSym "->")

literal :: ISParser s m => m Lit
literal = satisfy test >>= extract
  where test (TkLit _) = True
        test _         = False
        extract (TkLit x, _) = return x
        extract _            = impossible

--  ### Combinators
-- #### Concrete Combinators
pLitCon, pVar, pCon :: ISParser s m => m Var
pLitCon =  MP.try (paren pTupleCon)
       <|> MP.try (paren $ return (ConSym "()"))
       <|> lsquar *> rsquar *> return (ConSym "[]")
  where
    pTupleCon = MP.some comma >>= (return . mkTuple . length)
    mkTuple n = ConSym $ "(" ++ replicate n ',' ++ ")"

pVar = varid <|> MP.try (paren varsym)
pCon = conid <|> MP.try (paren consym') <|> pLitCon
  where
    consym' = consym <|> conCons

pEOp, pPOp, pTOp :: ISParser s m => m (AnnotPos Var)
pEOp = scope' (opid <|> opsym)
  where opid  = backtick *> (varid <|> conid) <* backtick
        opsym = varsym <|> consym

pPOp = scope' (opConId <|> opConSym)
  where opConId  = backtick *> conid <* backtick
        opConSym = consym <|> conCons

pTOp = pPOp <|> scope' conArrow

pWhere :: ISParser s m => m [PosedDecl]
pWhere = toList <$> optional (reserved "where" *> braceBlock pDecl)
  where
    toList Nothing   = []
    toList (Just xs) = xs

-- #### Abstract Combinators
pFXs' pf px fxs = scope $ do
  f <- pf; xs <- MP.many px
  if null xs then return $ snd $ out f
             else return $ fxs f xs

pFXs pa fxs = pFXs' pa pa fxs

-- TODO: wait until the next release of megaparsec.
sepEndBy p sep = sepEndBy1 p sep <|> pure []
sepEndBy1 p sep = (:) <$> p <*> ((sep *> sepEndBy p sep) <|> pure [])
--    # Non-trivial Combinators
--   ## Type
pType, pFType, pAType :: ISParser s m => m PosedType
pType = pScheme <|> pInfix
  where
    pScheme = let
      env = reserved "forall" *> MP.many (scope' varid) <* dotsign
      ty  = pInfix
      in scope $ liftA2 TScheme env ty

    pInfix = MP.try pOpType <|> pFType
      where pOpType = scope $ liftA3 TInfix pFType pTOp pType

pFType = pFXs pAType TApp

pAType = scope $ pTCon <|> pTVar <|> pParened
  where
    pTCon = fmap TCon (pCon <|> fmap snd (MP.try (paren pTOp)))
    pTVar = fmap TVar varid
    pParened = fmap TParen (paren pType)

pTyVar :: ISParser s m => m PosedType
pTyVar = scope $ fmap TVar varid

--   ## Pattern
pPat, pLPat, pFPat, pAPat :: ISParser s m => m PosedPat
pPat = MP.try pOpCon <|> pLPat
  where
    pOpCon = scope $ liftA3 PInfix pLPat pPOp pPat

pLPat = pFPat <|> pAPat <|> pNeg
  where
    pNeg = scope $ do _   <- negsign
                      lit <- literal
                      case lit of
                        LInt   n -> return $ PLit $ LInt   (-n)
                        LFloat n -> return $ PLit $ LFloat (-n)
                        _        -> fail "expect a number."

pFPat = scope $ liftA2 PConApp pPOp (MP.many pAPat)

-- TODO: add support for tuple, list, labeled pattern, and irrefutable pattern
pAPat = scope (MP.try pPVar <|> pAs <|> pPLit <|> pP_ <|> pParened)
  where
    pPVar = fmap PVar varid
    pAs   = liftA2 PAs (scope' varid <* reserved "*") pAPat
    pPLit = fmap PLit literal
    pP_   = reserved "_" *> return PWildcard
    pParened = fmap PParen (paren pPat)

--   ## Expression
pExpr, pLExpr, pFExpr, pAExpr :: ISParser s m => m PosedExpr
-- TODO: add support for type ascription, exp :: type
pExpr = MP.try pOpExpr <|> pNeg <|> pLExpr
  where
    pOpExpr = scope $ liftA3 EInfix pLExpr pEOp pExpr
    pNeg = scope $ fmap ENeg (negsign *> pExpr)

-- TODO: add support for do-notation [after typeclass]
pLExpr = pLExpr' <|> pFExpr
  where
    pLExpr' = scope $ pLam <|> pLet <|> pIf <|> pCase

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
      where pAlt = liftA3 (,,) pPat (sarrow *> pExpr) pWhere

pFExpr = pFXs pAExpr EApp

-- TODO: add support for tuple, list, labeled construction and update, and
--       consider whether to add arithmetic seqeunce and list comprehension
pAExpr = scope $  pEVar    <|> pECon   <|> pELit
              <|> pParened <|> pLOpSec <|> pROpSec
  where
    pEVar = fmap EVar pVar
    pECon = fmap ECon pCon
    pELit = fmap ELit literal
    pParened = fmap EParen $ paren pExpr
    pLOpSec = paren $ liftA2 (flip ELOpSec) pExpr pEOp
    pROpSec = paren pROpSec'
      where pROpSec' = do op <- pEOp
                          x  <- pExpr
                          case op of
                            (_, VarSym "-") -> return $ ENeg x
                            _               -> return $ EROpSec op x

--    # Declaration
--   ## Type, Newtype, Data
-- TODO: add support for type class.
pTypeAlias, pNewType, pDataType :: ISParser s m => m PosedDecl
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
    pRhs = equal *> (MP.sepBy1 pConDef $ reserved "|")
      where
        pConDef = MP.try pInfix <|> pPrefix
        pInfix  = scope $ liftA3 TInfix pFType pTOp pFType
        pPrefix = pFXs' pTCon pAType TApp
        pTCon   = scope $ fmap TCon pCon

pSimpleType :: ISParser s m => m PosedType
pSimpleType = pPrefix <|> pInfix
  where pPrefix = pFXs' pSTCon pTyVar TApp
        pInfix = scope $ liftA3 TInfix pTyVar pTOp pTyVar
        pSTCon = scope $ fmap TCon pCon

--   ## Function Declaration
pGenDecl :: ISParser s m => m PosedDecl
pGenDecl = scope $ pTypeSig <|> pFixity
  where
-- TODO: add support for type signature with context.
    pTypeSig = liftA2 DeclTypeSig pVars (reserved "::" *> pType)
      where pVars = MP.sepBy1 (scope' pVar) comma
    pFixity  = liftA3 DeclFixity pAssocType pFixityLv pOps
      where
        pFixityLv = MP.optional pInt >>=
                    return . (\case Nothing -> 9; Just l -> l)
        pAssocType =  reserved "infixl" *> return InfixL
                  <|> reserved "infixr" *> return InfixR
                  <|> reserved "infix"  *> return Infix

        pInt :: ISParser s m => m Int
        pInt = satisfy test >>= extract
          where test (TkLit _) = True
                test _         = False
                extract (TkLit (LInt x), _) = return x
                extract _                   = impossible

        pOps = MP.sepBy1 (pEOp <|> pPOp) comma

pDecl :: ISParser s m => m PosedDecl
pDecl =  MP.try pGenDecl <|> pDecl'
  where
    pDecl' = scope $ liftA3 DeclEqt pLhs (equal *> pRhs) pWhere
    pLhs = scope' $ MP.try pOpLhs <|> pLLhs
      where
        pOpLhs = liftA3 FnOp pPat pEOp pPat
        pLLhs  = p1 <|> p2
          where
            p1 = liftA2 FnArgs (scope' pVar) (MP.many pAPat)
            p2 = do (FnArgs f xs) <- paren p1
                    rest          <- MP.some pAPat
                    return $ FnArgs f (xs ++ rest)

-- TODO: add support for guard expression.
    pRhs = pExpr

pTopDecl :: ISParser s m => m PosedDecl
pTopDecl = pTypeAlias <|> pNewType <|> pDataType <|> pDecl
--   ## Module
pModule :: ISParser s m => m [PosedDecl]
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

parse :: String -> String -> [PosedDecl]
parse = parse' (pModule <* MP.eof)
