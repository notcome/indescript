{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Indescript.Parser where

import qualified Text.Megaparsec           as P
import qualified Text.Megaparsec.Pos       as Pos
import           Text.Megaparsec.Prim      (MonadParsec)
import           Text.Megaparsec.Error
import           Text.Megaparsec.ShowToken

import Language.Indescript.Syntax
import Language.Indescript.Lexer

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

primitive :: MonadParsec s m PosToken => m PosToken
primitive = satisfy test
  where test (TkLiteral _) = True
        test (TkVarId   _) = True
        test (TkVarSym  _) = True
        test (TkConId   _) = True
        test (TkConSym  _) = True
        test _             = False

space :: MonadParsec s m PosToken => m PosToken
space = satisfy test
  where test TkWhite   = True
        test TkComment = True
        test _         = False

expr :: MonadParsec s m PosToken => m (Expr TokenPos)
expr = do tokens <- P.some (primitive <* P.many space)
          error $ show tokens

test :: String -> P.Parsec [PosToken] (Expr TokenPos)	-> Either P.ParseError (Expr TokenPos)
test input parser = let (Right lexed) = Language.Indescript.Lexer.lex input
                    in P.parse parser "(source)" lexed
