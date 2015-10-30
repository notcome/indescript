{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind     #-}

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.Indescript.Parser where

import Control.Applicative

import           Text.Megaparsec.ShowToken
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

backtick = reserved "`"

sarrow = reserved "->"
darrow = reserved "=>"

--  ### Combinators
paren p = lparen *> p <* rparen

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

pOp :: ISParser s m => m (Op ElemPos)
pOp = scope $ (oid <|> sym) >>= return . Op
  where oid = backtick *> varid <|> conid <* backtick
        sym = varsym <|> consym

pConOp :: ISParser s m => m (Op ElemPos)
pConOp = scope $ (oid <|> sym) >>= return . Op
  where oid = backtick *> varid <|> conid <* backtick
        sym = varsym <|> consym

pTyConOp :: ISParser s m => m (Op ElemPos)
pTyConOp = pConOp <|> arrowOp
  where arrowOp = scope $ sarrow >>= extractTkVar >>= return . Op

-- #### Abstract Combinators

--    # Non-trivial Combinators
--   ## Type
pType :: ISParser s m => m (Type ElemPos)
pType = pForall <|> pInfix
  where
    pForall = do env <- reserved "forall" *> MP.many pVar <* dotsign
                 ty  <- pInfix
                 return $ TForall env ty $ elemPos (env, ty)

    pInfix = pOpType <|> pFType
      where pOpType = scope $ liftA3 TInfix pFType pTyConOp pType

    pFType = do fxs <- MP.some pAType
                case fxs of
                  [f]    -> return f
                  (f:xs) -> return $ TApp f xs $ elemPos fxs
                  []     -> impossible

    pAType = pCon <|> pVar <|> pParened
      where
        pCon = scope $ fmap TCon con'
          where con' = con <|> paren sarrow *> return (ConSym "->")
        pParened = scope $ paren pType >>= (return . flip updateAST)

    pVar = scope $ fmap TVar varid

--   ## Pattern
pPat :: ISParser s m => m (Pat ElemPos)
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

pAPat :: ISParser s m => m (Pat ElemPos)
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

    pFExpr = do fxs <- MP.some pAExpr
                case fxs of
                  [f]    -> return f
                  (f:xs) -> return $ EApp f xs $ elemPos fxs
                  []     -> impossible

-- TODO: add support for tuple, list, labeled construction and update, and
--       consider whether to add arithmetic seqeunce and list comprehension
    pAExpr = pVar <|> pCon <|> pLit <|> pParened <|> pLOpSec <|> pROpSec
      where
        pVar = scope $ fmap EVar var
        pCon = scope $ fmap ECon con
        pLit = scope $ fmap ELit literal
        pParened = scope $ paren pExpr >>= (return . flip updateAST)
        pLOpSec = scope $ paren parser
          where parser = do x  <- pInfix
                            op <- pOp
                            return $ ELOpSec op x
        pROpSec = scope $ paren parser
          where parser = do op@(Op opv _) <- pOp
                            x <- pInfix
                            case opv of
                              VarSym "-" -> return $ ENeg x
                              _          -> return $ EROpSec op x

--    # Interface
{-
parse' parser src input = case lexSource src input of
  (Left errMsg) -> Left $ show errMsg
  (Right lexed) -> let lexed' = insertSemicolonBraces lexed
    in case MP.parse parser src lexed' of
      (Left errMsg)  -> Left $ show errMsg
      (Right parsed) -> Right parsed
  where
    -- TODO: implement the semicolon brace insertion algorithm
    insertSemicolonBraces = id

parse = parse' pAtom

-}
