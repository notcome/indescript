{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Indescript.Parser where

import qualified Text.Megaparsec           as P
import           Text.Megaparsec           ((<?>))
import qualified Text.Megaparsec.Pos       as Pos
import           Text.Megaparsec.Prim      (MonadParsec)
import           Text.Megaparsec.Error
import           Text.Megaparsec.ShowToken

import Language.Indescript.Syntax
import Language.Indescript.Parser.Lexer

updatePosToken :: Int -> P.SourcePos -> PosToken -> P.SourcePos
updatePosToken _ pos (tokenPos, _) = let
  source = Pos.sourceName pos
  line   = tokenLine   tokenPos
  column = tokenColumn tokenPos + tokenWidth tokenPos
  in Pos.newPos source line column

instance ShowToken Token where
  showToken = show

instance ShowToken PosToken where
  showToken = show

instance ShowToken [PosToken] where
  showToken = show

nextToken = P.token updatePosToken

token :: MonadParsec s m PosToken => m PosToken
token = P.token updatePosToken Right

satisfy' :: MonadParsec s m PosToken => (PosToken -> Bool) -> m PosToken
satisfy' p = P.token updatePosToken test
  where test pt@(_, t) = if p pt
                         then Right pt
                         else Left $ pure $ Unexpected $ showToken t

satisfy :: MonadParsec s m PosToken => (Token -> Bool) -> m PosToken
satisfy p = satisfy' p' where p' (_, t) = p t

symbol :: MonadParsec s m PosToken => Token -> m PosToken
symbol token = satisfy (== token)

unexpected = Left . pure . Unexpected

primitive :: MonadParsec s m PosToken => m (Expr TokenPos)
primitive = nextToken $ layer pick
  where
    layer f (pos, tok) = case f tok of
      Just sth -> Right $ sth pos
      Nothing  -> Left . pure . Unexpected $ showToken tok

    pick (TkLiteral x) = Just $ ELit x
    pick (TkVarId   x) = Just $ EVar $ EVStr x
    pick (TkVarSym  x) = Just $ EVar $ EVSym x
    pick (TkConId   x) = Just $ ECon (EVStr x, undefined)
    pick (TkConSym  x) = Just $ ECon (EVSym x, undefined)
    pick _             = Nothing

space :: MonadParsec s m PosToken => m PosToken
space = satisfy test
  where test TkWhite   = True
        test TkComment = True
        test _         = False

spaces = P.many space

expr :: MonadParsec s m PosToken => m (Expr TokenPos)
expr = do tokens <- P.some (primitive <* spaces)
          error $ show tokens
  where
    isOp (EVar (EVSym _)    _) = True
    isOp (ECon (EVSym _, _) _) = True
    isOp _                      = False

    buildFuncApp [x]    = return x
    buildFuncApp (f:xs) = return $ EApp f xs $ getSourcePos (f, xs)

    build' []  []       = error "impossible happens: zero token"
    build' []  (op:_)   = fail "lhs operand not found" -- at " ++ fmap show op we need to provide a join/getInfo
    build' _   (op:[])  = fail "rhs operand not found" -- at " ++ fmap show op
    build' fxs []       = buildFuncApp fxs
    build' lhs (op:rhs) = do
      lhs' <- buildFuncApp lhs
      rhs' <- build rhs
      return $ EApp op [lhs', rhs'] $ getSourcePos (lhs, op:rhs)

    build xs = let (lhs, oprhs) = break isOp xs in build' lhs oprhs

type SourcePos = TokenPos

class GetSourcePos a where
  getSourcePos :: a -> SourcePos

instance GetSourcePos (Expr TokenPos) where
  getSourcePos = undefined

instance (GetSourcePos a) => GetSourcePos [a] where
  getSourcePos = undefined

instance (GetSourcePos a, GetSourcePos b) => GetSourcePos (a, b) where
  getSourcePos = undefined

test :: String -> P.Parsec [PosToken] (Expr TokenPos) -> Either P.ParseError (Expr TokenPos)
test input parser = let (Right lexed) = Language.Indescript.Parser.Lexer.lex input
                    in P.parse parser "(source)" lexed
