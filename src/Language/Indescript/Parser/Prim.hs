{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.Indescript.Parser.Prim where

import Control.Applicative
import Data.Annotation
import Data.Convertible

import qualified Text.Megaparsec           as MP
import           Text.Megaparsec.Prim      (MonadParsec)
import           Text.Megaparsec.ShowToken

import Language.Indescript.Syntax
import Language.Indescript.Parser.SourcePos
import Language.Indescript.Parser.Lexer

nextToken :: MonadParsec s m PosToken
          => (PosToken -> Either [MP.Message] a) -> m a
nextToken = MP.token updateTokenPos
  where updateTokenPos _ _ (_, pos) = convert $ advanceSourcePos pos

satisfy' :: MonadParsec s m PosToken => (PosToken -> Bool) -> m PosToken
satisfy' test = nextToken (\pt@(t, _) ->
  if test pt then Right pt
             else Left $ pure $ MP.Unexpected $ showToken t)

satisfy :: MonadParsec s m PosToken => (Token -> Bool) -> m PosToken
satisfy test = satisfy' test' where test' (t, _) = test t

token :: MonadParsec s m PosToken => Token -> m PosToken
token t = satisfy (== t)

space' = MP.many (token TkWhite <|> token TkComment)
space  = MP.many (token TkWhite <|> token TkComment <|> token TkNewline)

reserved = token . TkReserved

lparen = token $ TkLParen CircleParen
rparen = token $ TkRParen CircleParen
lbrace = token $ TkLParen CurlyParen
rbrace = token $ TkRParen CurlyParen

class GetSourceRange a where
  getSourceRange :: a -> SourcePos

instance GetSourceRange SourcePos where
  getSourceRange = id

instance GetSourceRange a => GetSourceRange (Pattern a) where
  getSourceRange = annotation . fmap getSourceRange

instance GetSourceRange a => GetSourceRange (Branch a) where
  getSourceRange = annotation . fmap getSourceRange

instance GetSourceRange a => GetSourceRange (Equation a) where
  getSourceRange = annotation . fmap getSourceRange

instance GetSourceRange a => GetSourceRange (Expr a) where
  getSourceRange = annotation . fmap getSourceRange

instance (GetSourceRange a, GetSourceRange b) => GetSourceRange (a, b) where
  getSourceRange (l, r) = let
    (SourcePos n ll lc _)        = getSourceRange l
    (SourcePos _ rl rc (dl, dc)) = getSourceRange r
    in SourcePos n ll lc (rl + dl - ll, rc + dc - lc)

instance GetSourceRange a => GetSourceRange [a] where
  getSourceRange = foldl1 combine . map getSourceRange
    where combine l r = getSourceRange (l, r)
