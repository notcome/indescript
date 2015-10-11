{-# LANGUAGE FlexibleContexts #-}

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

token :: MonadParsec s m PosToken => Token -> m Token
token t = P.token updatePosToken testToken
  where testToken (_, x) = if x == t
                           then Right t
                           else Left $ pure $ Unexpected $ showToken x
