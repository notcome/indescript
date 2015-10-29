{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind     #-}

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.Indescript.Parser where

import Control.Applicative
import Data.Annotation

import           Text.Megaparsec.ShowToken
import qualified Text.Megaparsec      as MP
import           Text.Megaparsec.Prim (MonadParsec)

import Language.Indescript.Syntax
import Language.Indescript.Parser.Prim
import Language.Indescript.Parser.Pos
import Language.Indescript.Parser.Lexer

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

type PPattern = Pattern SourcePos
type PExpr    = Expr    SourcePos

pAtom :: MonadParsec s m PosedToken => m PExpr
pAtom = pIf <|> pLit <|> pVar <|> pLam <|> pCase <|> pParened -- pLet <|>

pParened :: MonadParsec s m PosedToken => m PExpr
pParened = do (_, sp) <- lparen <* space
              (e, fl) <- resolve =<< MP.many (pAtom' <* space)
              (_, ep) <- space *> rparen
              if fl then return $ fmap (const $ elemPos (sp, ep)) e
                    else return e
  where
    pAtom' = nakedOp <|> pAtom
      where nakedOp = do (op, pos) <- pEOp; return $ EAtom (EOp op) pos

    isOp (EAtom (EOp _) _) = True
    isOp _                 = False
    notOp                  = not . isOp

    resolve []    = return (EAtom (ELit LUnit) undefined, True)
    resolve [x]
      | isOp x    = return (x, True)
      | otherwise = return (x, False)
    resolve [x, y]
      | isOp x && notOp y, EAtom (EOp op) p <- x =
        return (EOpSec (op, p) y SecRight undefined, True)
      | isOp y && notOp x, EAtom (EOp op) p <- y =
        return (EOpSec (op, p) x SecLeft  undefined, True)
    resolve l = do res <- build' l; return (res, False)
      where
        build' xs     = let (lhs, op_rhs) = break isOp xs in build lhs op_rhs
        appFn  []     = error "Impossible happens!"
        appFn  [f]    = return f
        appFn  (f:xs) = return $ EApp f xs $ elemPos (f:xs)

        build []  []       = error "Impossible happens!"
        build []  (op:_)   = fail $ "Left operand not found at "  ++ (show . annotation) op
        build _   (op:[])  = fail $ "Right operand not found at " ++ (show . annotation) op
        build fxs []       = appFn fxs
        build lhs (op:rhs) = do
          lhs' <- appFn  lhs
          rhs' <- build' rhs
          return $ EApp op [lhs', rhs'] $ elemPos (lhs', rhs')

pIf :: MonadParsec s m PosedToken => m PExpr
pIf = do (_, sp) <- reserved "if"   <* space
         cond    <- pAtom <* space
         _       <- reserved "then" <* space
         exp1    <- pAtom <* space
         _       <- reserved "else" <* space
         exp2    <- pAtom
         return $ EIf cond exp1 exp2 $ elemPos (sp, [exp1, exp2])

pLam :: MonadParsec s m PosedToken => m PExpr
pLam = do (_, sp) <- reserved "\\" <* space
          px      <- pPattern
          pxs     <- MP.many (space *> pPattern)
          space *> reserved "->" <* space
          expr    <- pAtom
          return $ ELam (px:pxs) expr $ elemPos (sp, expr)

-- TODO: insert semicolon and braces
pCase :: MonadParsec s m PosedToken => m PExpr
pCase = do (_, sp) <- reserved "case" <* space
           expr    <- pAtom <* space
           reserved "of" >> space >> lbrace >> space
           brchs   <- MP.many (branch <* space <* token TkSemicolon <* space)
           (_, ep) <- space *> rbrace
           return $ ECase expr brchs $ elemPos (sp, ep)
  where branch = do pat <- pPattern
                    space *> reserved "->" <* space
                    expr <- pAtom
                    return $ Branch pat expr $ elemPos (pat, expr)

nodify :: MonadParsec s m PosedToken
       => m (a, SourcePos)
       -> (a -> SourcePos -> b)
       -> m b
nodify parser con = do (x, pos) <- parser; return $ con x pos

pLit = nodify pELit  EAtom
pVar = nodify pEVVar EAtom

pELit :: MonadParsec s m PosedToken => m (EAtom, SourcePos)
pELit = do (TkLiteral lit, pos) <- satisfy isLiteral
           return (ELit lit, pos)
  where isLiteral (TkLiteral _) = True
        isLiteral _             = False

pEVVar :: MonadParsec s m PosedToken => m (EAtom, SourcePos)
pEVVar = do (tok, pos) <- satisfy isVarCon
            let atom = case tok of TkVarId x -> x
                                   TkConId x -> x
                                   _         -> error "Impossible happens!"
            return (EVar $ EVVar atom, pos)
  where isVarCon (TkVarId _) = True
        isVarCon (TkConId _) = True
        isVarCon _           = False

pEOp :: MonadParsec s m PosedToken => m (EOp, SourcePos)
pEOp = sym <|> var
  where
    isSymbol (TkVarSym _) = True
    isSymbol (TkConSym _) = True
    isSymbol _            = False
    backtick              = token TkBacktick

    sym = do (tok, pos) <- satisfy isSymbol
             let op = case tok of TkVarSym x -> x
                                  TkConSym x -> x
                                  _          -> error "Impossible happens!"
             return (EOOp op, pos)

    var = do (_, sp) <- backtick <* space'
             (EVar (EVVar tok), _) <- pEVVar
             (_, ep) <- space' *> backtick
             return (EOVar tok, elemPos (sp, ep))

pPattern :: MonadParsec s m PosedToken => m PPattern
pPattern = atom <|> discarded <|> decon <|> binded
  where
    atom      = nodify (pELit <|> pEVVar) PAtom
    discarded = (PDiscard . snd) <$> reserved "_"

    decon  = do (_, sp) <- lparen
                subcon  <- pPCon <|> pPConOp
                (_, ep) <- rparen
                return $ subcon $ elemPos (sp, ep)
      where
        pPCon   = do (EVar con, cp) <- pEVVar
                     binds          <- MP.many (space *> pPattern) <* space
                     return $ PCon (con, cp) binds
        pPConOp = do pat1 <- pPattern <* space
                     op   <- pEOp <* space
                     pat2 <- pPattern
                     return $ PConOp op pat1 pat2
    binded = do (EVar var, sp) <- pEVVar
                pat            <- space' *> reserved "@" *> space' *> decon
                return $ PBinding (var, sp) pat $ elemPos (sp, pat)
-}


----

pExpr :: ISParser s m => m (Expr ElemPos)
-- TODO: add support for type ascription, exp :: type
pExpr = pInfix
  where
    pInfix :: ISParser s m => m (Expr ElemPos)
    pInfix = pOpExpr <|> pNeg <|> pLExpr
      where
        pOpExpr = do lhs <- pLExpr
                     op  <- pOp
                     rhs <- pInfix
                     return $ EInfix op lhs rhs $ elemPos (lhs, rhs)
        pNeg = scope $ fmap ENeg (satisfy (== TkVar (VarSym "-")) *> pInfix)

-- TODO: add support for do-notation [after typeclass]
    pLExpr = pLam <|> pLet <|> pIf <|> pCase {-<|> pDo-} <|> pFExpr
      where
        pLam  = undefined
        pLet  = undefined
        pIf   = undefined
        pCase = undefined

    pFExpr = do fxs <- MP.some pAExpr
                case fxs of
                  (f:xs) -> return $ EApp f xs $ elemPos fxs
                  [f]    -> return f
                  []     -> impossible

-- TODO: add support for tuple, list, labeled construction and update, and
--       consider whether to add arithmetic seqeunce and list comprehension
    pAExpr = pVar <|> pCon <|> pLit <|> pParened <|> pLOpSec <|> pROpSec
      where
        pVar = let var = scope $ fmap EVar varid
                   sym = scope $ fmap EVar (lparen *> varsym <* rparen)
                in var <|> sym

        pCon = let var = scope $ fmap ECon conid
                   sym = scope $ fmap ECon (lparen *> consym <* rparen)
                   lit =  scope (lparen >> rparen >> litCon "()")
                      <|> scope (lsquar >> rsquar >> litCon "[]")
                      <|> scope (do _  <- lparen
                                    xs <- MP.some $ satisfy (== TkRsv ",")
                                    _  <- rparen
                                    let xs' = nTupleCon $ length xs
                                    return $ ECon (ConSym xs'))
                   litCon = return . ECon . ConSym
-- TODO: move nTupleCon to a more general position.
                   nTupleCon n = "(" ++ replicate n ',' ++ ")"
                in var <|> sym
        pLit = scope $ fmap ELit literal
-- TODO: shall the position include parentheses?
        pParened = scope $ do e <- lparen *> pExpr <* rparen
                              return $ flip updateAST e
        pLOpSec = scope $ lparen *> parser <* rparen
          where parser = do x  <- pInfix
                            op <- pOp
                            return $ ELOpSec op x
        pROpSec = scope $ lparen *> parser <* rparen
          where parser = do op@(EOp opv _) <- pOp
                            x <- pInfix
                            case opv of
                              VarSym "-" -> return $ ENeg x
                              _          -> return $ EROpSec op x

    pOp :: ISParser s m => m (Expr ElemPos)
    pOp = let var = scope $ fmap EOp (backtick *> varid <|> conid <* backtick)
              sym = scope $ fmap EOp (varsym <|> consym)
          in var <|> sym

{-
|	( infixexp qop )	    (left section)
|	( qop⟨-⟩ infixexp )	    (right section)


%simpletype = $tycon $tyvar*

%type = btype (-> type)
%btype = (btype) atype
%atype  = gtycon
        | tyvar
        | (type, type, …)
        | [type]
        | (type)

gtycon = $tycon
       | () | [] | (->) -> (,+)

pTyVar
pTyCon

pSimpleType =
-}
