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

import qualified Text.Megaparsec      as MP

import Language.Indescript.Syntax
import Language.Indescript.Parser.Prim
import Language.Indescript.Parser.Pos
import Language.Indescript.Parser.Lexer

--    # Primitives
--   ## Special Characters
--  ### Literal Values
lparen = reserved "("
rparen = reserved "("
lsquar = reserved "["
rsquar = reserved "]"
lbrace = reserved "{"
rbrace = reserved "}"

negsign = token $ TkVar $ VarSym "-"
dotsign = token $ TkVar $ VarSym "."

backtick  = reserved "`"
semicolon = reserved ";"
comma     = reserved ","

equal  = reserved "="
sarrow = reserved "->"
darrow = reserved "=>"

--  ### Combinators
paren p = lparen *> p <* rparen
brace p = lbrace *> p <* rbrace

-- { p1; p2; ... }
braceBlock p = brace $ MP.many $ p <* semicolon

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
var :: ISParser s m => m Variable
var = varid <|> paren varsym

con :: ISParser s m => m Variable
con = conid <|> paren consym <|> lit
  where lit =  lparen *> rparen *> return (ConSym "()")
           <|> lsquar *> rsquar *> return (ConSym "[]")
           <|> paren pTupleCon
        pTupleCon = MP.some (reserved ",") >>= (return . mkTuple . length)
-- TODO: move mkTuple to a more general position.
        mkTuple n = ConSym $ "(" ++ replicate n ',' ++ ")"

pOp, pConOp, pTyConOp :: ISParser s m => m (Op ElemPos)
pOp = scope $ (oid <|> sym) >>= return . Op
  where oid = backtick *> varid <|> conid <* backtick
        sym = varsym <|> consym

pConOp = scope $ (oid <|> sym) >>= return . Op
  where oid = backtick *> varid <|> conid <* backtick
        sym = varsym <|> consym

pTyConOp = pConOp <|> arrowOp
  where arrowOp = scope $ sarrow >>= extractTkVar >>= return . Op

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

    pInfix = pOpType <|> pFType
      where pOpType = scope $ liftA3 TInfix pFType pTyConOp pType

pFType = pFXs pAType pAType TApp

pAType = pCon <|> pTyVar <|> pParened
  where
    pCon = scope $ fmap TCon con'
      where con' = con <|> paren sarrow *> return (ConSym "->")
    pParened = scope $ paren pType >>= (return . flip updateAST)

pTyVar :: ISParser s m => m (Type ElemPos)
pTyVar = scope $ fmap TVar varid

--   ## Pattern
pPat, pAPat :: ISParser s m => m (Pat ElemPos)
pPat = pInfix <|> pLPat
  where
    pInfix = pOpCon <|> pLPat
      where pOpCon = scope $ liftA3 PInfix pLPat pConOp pPat

    pLPat = pAPat <|> pNeg <|> pFPat
      where
        pNeg = scope $ do _   <- negsign
                          lit <- literal
                          case lit of
                            LInt   n -> return $ PLit $ LInt   (-n)
                            LFloat n -> return $ PLit $ LFloat (-n)
                            _        -> fail "expected a number."
        pFPat = scope $ do f  <- scope $ fmap PCon con
                           xs <- MP.some pAPat
                           return $ PApp f xs

-- TODO: add support for tuple, list, labeled pattern, and irrefutable pattern
pAPat = pAs <|> pAs' <|> pCon <|> pLit <|> pWildcard <|> pParened
  where
    pAs  = scope $ fmap PVar var
    pAs' = scope $ liftA2 PAs pAs (reserved "a" *> pAPat)
    pCon = scope $ fmap PCon con
    pLit = scope $ fmap PLit literal
    pWildcard = scope $ reserved "_" *> return PWildcard
    pParened  = scope $ paren pPat >>= return . flip updateAST

--   ## Expression
pExpr :: ISParser s m => m (Expr ElemPos)
-- TODO: add support for type ascription, exp :: type
pExpr = pInfix
  where
    pInfix = pOpExpr <|> pNeg <|> pLExpr
      where
        pOpExpr = scope $ liftA3 EInfix pLExpr pOp pInfix
        pNeg = scope $ fmap ENeg (negsign *> pInfix)

-- TODO: add support for do-notation [after typeclass]
    pLExpr = pLam <|> pLet <|> pIf <|> pCase {-<|> pDo-} <|> pFExpr
      where
        pLam  = undefined
        pLet  = undefined
        pIf   = undefined
        pCase = undefined

    pFExpr = pFXs pAExpr pAExpr EApp

-- TODO: add support for tuple, list, labeled construction and update, and
--       consider whether to add arithmetic seqeunce and list comprehension
    pAExpr = scope $ pVar <|> pCon <|> pLit <|> pParened <|> pLOpSec <|> pROpSec
      where
        pVar = fmap EVar var
        pCon = fmap ECon con
        pLit = fmap ELit literal
        pParened = paren pExpr >>= (return . flip updateAST)
        pLOpSec = paren parser
          where parser = do x  <- pInfix
                            op <- pOp
                            return $ ELOpSec op x
        pROpSec = paren parser
          where parser = do op@(Op opv _) <- pOp
                            x <- pInfix
                            case opv of
                              VarSym "-" -> return $ ENeg x
                              _          -> return $ EROpSec op x

--    # Declaration
--   ## Type, Newtype, Data
-- TODO: add support for type class.

pTypeAlias, pNewType, pDataType :: ISParser s m => m (Decl ElemPos)
pTypeAlias = scope $ liftA2 DeclTypeAlias pLhs pType
  where
    pLhs = reserved "type" *> pSimpleType

-- TODO: add support for the field-like syntax.
pNewType = scope $ do tyName  <- reserved "newtype" *> pSimpleType
                      _       <- equal
                      conName <- scope $ fmap TCon conid
                      wrapped <- pAType
                      return $ DeclNewType tyName conName wrapped

pDataType = scope $ liftA2 DeclDataType pLhs pRhs
  where
    pLhs = reserved "data" *> pSimpleType
    pRhs = equal *> (MP.sepBy1 pConstr $ reserved "|")
      where
        pConstr = pLConstr <|> pInfix
          where
            pLConstr = pFXs pCon pArg TApp
            pInfix = scope $ liftA3 TInfix pArg pConOp pArg
            pCon = scope $ fmap TCon con
            pArg = pFType <|> pAType

pSimpleType :: ISParser s m => m (Type ElemPos)
pSimpleType = pPrefix <|> pInfix
  where pPrefix = pFXs (scope $ fmap TCon conid) pTyVar TApp
        pInfix = scope $ liftA3 TInfix pTyVar pConOp pTyVar

--   ## Function Declaration
pGenDecl :: ISParser s m => m (Decl ElemPos)
pGenDecl = scope $ pTypeSig <|> pFixity
  where
-- TODO: add support for type signature with context.
    pTypeSig = liftA2 DeclTypeSig pVars pType
      where pVars = MP.sepBy1 (scope $ fmap Var var) comma
    pFixity  = fmap DeclFixity pFixity'
      where
        pFixity' = scope $  liftA2 Fixity pAssocType pOps
                        <|> liftA3 FixityLv pAssocType pInt pOps

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
pDecl =  pGenDecl <|> pDecl'
  where
    pDecl' = scope (liftA3 mkDeclFn pLhs (equal *> pRhs) (optional pWhere))

    mkDeclFn lhs rhs Nothing   = DeclFn lhs rhs []
    mkDeclFn lhs rhs (Just ws) = DeclFn lhs rhs ws

    pLhs = pOpLhs <|> pLLhs
      where
        pOpLhs = scope $ liftA3 FnOp pPat pOp pPat
        pLLhs  = p1 <|> p2
          where
            p1 = scope $ liftA2 FnArgs var' (MP.some pAPat)
              where var' = scope $ fmap Var var
            p2 = scope $ do (FnArgs f xs _) <- paren p1
                            rest            <- MP.some pAPat
                            return $ FnArgs f (xs ++ rest)

-- TODO: add support for guard expression.
    pRhs = pExpr
    pWhere = reserved "where" *> braceBlock pDecl

--   ## Module
pModule :: ISParser s m => m [Decl ElemPos]
pModule = reserved "module" *> reserved "where" *> braceBlock pDecl

--    # Interface
parse :: String -> String -> [Decl ElemPos]
parse srcName input = let
  (Just tokens)   = lexIndescript input
  stateMonad      = MP.runParserT pModule srcName tokens
  (Right program) = evalState stateMonad $ ElemPos pesudoPoint zeroSpan
  in program
