{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Indescript.Parser.Prim where

import Control.Monad.State (get, put, MonadState(..))

import qualified Text.Megaparsec           as MP
import qualified Text.Megaparsec.Pos       as MPos
import           Text.Megaparsec.Prim      (MonadParsec)
import           Text.Megaparsec.ShowToken

import Language.Indescript.Syntax
import Language.Indescript.Parser.Pos
import Language.Indescript.Parser.Lexer

type ISParser s m = (MonadParsec s m PosedToken, MonadState ElemPos m)

nextToken :: ISParser s m
          => (PosedToken -> Either [MP.Message] PosedToken) -> m PosedToken
nextToken f = do (t, ps) <- MP.token updateTokenPos f
                 case ps of p:_ -> put p
                            []  -> impossible
                 return (t, ps)
  where updateTokenPos _ mp (_, (_:ps))
          | (p:_) <- ps = let
            name = MPos.sourceName mp
            in MPos.newPos name (startRow p) (startCol p)
          | []    <- ps = mp
        updateTokenPos _ _ _ = impossible

-- TODO: figure out a way to force GHC accept the truth.
scope' :: ISParser s m => m a -> m (AnnotPos a)
scope' p = do sPos <- nextElem >>= return . snd
              res  <- p
              ePos <- get
              return (elemPos (sPos, ePos), res)
  where nextElem = MP.lookAhead $ satisfy (const True)

scope :: ISParser s m => m (Outer AnnotPos f) -> m (Inner AnnotPos f)
scope p = do sPos <- nextElem >>= return . snd
             res  <- p
             ePos <- get
             return $ In (elemPos (sPos, ePos), res)
  where nextElem = MP.lookAhead $ satisfy (const True)

satisfy' :: ISParser s m => (PosedToken -> Bool) -> m PosedToken
satisfy' test = nextToken (\pt@(t, _) ->
  if test pt then Right pt
             else Left $ pure $ MP.Unexpected $ showToken t)

satisfy :: ISParser s m => (Token -> Bool) -> m PosedToken
satisfy test = satisfy' test' where test' (t, _) = test t

token :: ISParser s m => Token -> m PosedToken
token t = satisfy (== t)

reserved :: ISParser s m => String -> m PosedToken
reserved = token . TkRsv

type AnnotPos = (,) ElemPos

type PosedExpr = Expr AnnotPos
type PosedType = Type AnnotPos
type PosedPat  = Pat  AnnotPos
type PosedDecl = Decl AnnotPos

class GetElemPos a where
  elemPos :: a -> ElemPos

instance GetElemPos ElemPos where
  elemPos = id

instance GetElemPos (Fix AnnotPos f) where
  elemPos = fst . out

instance (GetElemPos a, GetElemPos b) => GetElemPos (a, b) where
  elemPos (l, r) = let
    lp = elemPos l; ls = startPoint lp; le = endPoint lp
    rp = elemPos r; rs = startPoint rp; re = endPoint rp
    ps = min ls rs
    pe = max le re
    in ElemPos ps (diffPoint ps pe)

instance GetElemPos a => GetElemPos [a] where
  elemPos = foldl1 combine . map elemPos
    where
      combine l r = elemPos (l, r)

instance ShowToken Token where
  showToken = show

instance ShowToken ElemPos where
  showToken = show

instance (ShowToken a, ShowToken b) => ShowToken (a, b) where
  showToken (a, b) = "(" ++ showToken a ++ ", " ++ showToken b ++ ")"

instance (ShowToken a) => ShowToken [a] where
  showToken []     = "[]"
  showToken [x]    = "[" ++ showToken x ++ "]"
  showToken (x:xs) = "[" ++ showToken x ++ concatMap ((", " ++) . showToken) xs ++ "]"

impossible = error "Confident impossibility."
