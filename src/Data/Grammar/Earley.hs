{-# LANGUAGE GADTs #-}

module Data.Grammar.Earley where

import Control.Applicative
import Text.Earley.Grammar hiding (Grammar)

import Control.SemiIso
import Data.Grammar

interpret :: Grammar t a -> Prod r e t a
interpret Void        = pure ()
interpret (Term p)    = satisfy p
interpret (f :$: cfg) = apply f <$> interpret cfg
interpret (g1 :*: g2) = (,) <$> interpret g1 <*> interpret g2
interpret (g1 :|: g2) = interpret g1 <|> interpret g2
