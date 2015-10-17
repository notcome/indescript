{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind     #-}

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.Indescript.Parser where

import Control.Applicative
import Data.Annotation

import qualified Text.Megaparsec      as MP
import           Text.Megaparsec.Prim (MonadParsec)

import Language.Indescript.Syntax
import Language.Indescript.Parser.Prim
import Language.Indescript.Parser.SourcePos
import Language.Indescript.Parser.Lexer

type PPattern = Pattern SourcePos
type PExpr    = Expr    SourcePos

pAtom :: MonadParsec s m PosToken => m PExpr
pAtom = pIf <|> pLit <|> pVar <|> pLam <|> pCase <|> pParened -- pLet <|>

pParened :: MonadParsec s m PosToken => m PExpr
pParened = do (_, sp) <- lparen <* space
              (e, fl) <- resolve =<< MP.many (pAtom' <* space)
              (_, ep) <- space *> rparen
              if fl then return $ fmap (const $ getSourceRange (sp, ep)) e
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
        appFn  (f:xs) = return $ EApp f xs $ getSourceRange (f:xs)

        build []  []       = error "Impossible happens!"
        build []  (op:_)   = fail $ "Left operand not found at "  ++ (show . annotation) op
        build _   (op:[])  = fail $ "Right operand not found at " ++ (show . annotation) op
        build fxs []       = appFn fxs
        build lhs (op:rhs) = do
          lhs' <- appFn  lhs
          rhs' <- build' rhs
          return $ EApp op [lhs', rhs'] $ getSourceRange (lhs', rhs')

pIf :: MonadParsec s m PosToken => m PExpr
pIf = do (_, sp) <- reserved "if"   <* space
         cond    <- pAtom <* space
         _       <- reserved "then" <* space
         exp1    <- pAtom <* space
         _       <- reserved "else" <* space
         exp2    <- pAtom
         return $ EIf cond exp1 exp2 $ getSourceRange (sp, [exp1, exp2])

pLam :: MonadParsec s m PosToken => m PExpr
pLam = do (_, sp) <- reserved "\\" <* space
          px      <- pPattern
          pxs     <- MP.many (space *> pPattern)
          space *> reserved "->" <* space
          expr    <- pAtom
          return $ ELam (px:pxs) expr $ getSourceRange (sp, expr)

-- TODO: insert semicolon and braces
pCase :: MonadParsec s m PosToken => m PExpr
pCase = do (_, sp) <- reserved "case" <* space
           expr    <- pAtom <* space
           reserved "of" >> space >> lbrace >> space
           brchs   <- MP.many (branch <* space <* token TkSemicolon <* space)
           (_, ep) <- space *> rbrace
           return $ ECase expr brchs $ getSourceRange (sp, ep)
  where branch = do pat <- pPattern
                    space *> reserved "->" <* space
                    expr <- pAtom
                    return $ Branch pat expr $ getSourceRange (pat, expr)

nodify :: MonadParsec s m PosToken
       => m (a, SourcePos)
       -> (a -> SourcePos -> b)
       -> m b
nodify parser con = do (x, pos) <- parser; return $ con x pos

pLit = nodify pELit  EAtom
pVar = nodify pEVVar EAtom

pELit :: MonadParsec s m PosToken => m (EAtom, SourcePos)
pELit = do (TkLiteral lit, pos) <- satisfy isLiteral
           return (ELit lit, pos)
  where isLiteral (TkLiteral _) = True
        isLiteral _             = False

pEVVar :: MonadParsec s m PosToken => m (EAtom, SourcePos)
pEVVar = do (tok, pos) <- satisfy isVarCon
            let atom = case tok of TkVarId x -> x
                                   TkConId x -> x
                                   _         -> error "Impossible happens!"
            return (EVar $ EVVar atom, pos)
  where isVarCon (TkVarId _) = True
        isVarCon (TkConId _) = True
        isVarCon _           = False

pEOp :: MonadParsec s m PosToken => m (EOp, SourcePos)
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
             return (EOVar tok, getSourceRange (sp, ep))

pPattern :: MonadParsec s m PosToken => m PPattern
pPattern = atom <|> discarded <|> decon <|> binded
  where
    atom      = nodify (pELit <|> pEVVar) PAtom
    discarded = (PDiscard . snd) <$> reserved "_"

    decon  = do (_, sp) <- lparen
                subcon  <- pPCon <|> pPConOp
                (_, ep) <- rparen
                return $ subcon $ getSourceRange (sp, ep)
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
                return $ PBinding (var, sp) pat $ getSourceRange (sp, pat)
