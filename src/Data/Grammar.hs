{-# LANGUAGE GADTs #-}

module Data.Grammar where

import Control.SemiIso

data Grammar t a where
  Void  :: Grammar t ()
  Term  :: (t -> Bool) -> Grammar t t
  (:$:) :: SemiIso a b -> Grammar t a -> Grammar t b
  (:*:) :: Grammar t a -> Grammar t b -> Grammar t (a, b)
  (:|:) :: Grammar t a -> Grammar t a -> Grammar t a

(/$/) :: SemiIso a b -> Grammar t a -> Grammar t b
(/$/) = (:$:)

(/*/) :: Grammar t a -> Grammar t b -> Grammar t (a, b)
(/*/) = (:*:)

(/|/) :: Grammar t a -> Grammar t a -> Grammar t a
(/|/) = (:|:)

infixr 6 :*:, /*/
infix  5 :$:, /$/
infixr 4 :|:, /|/

many :: Grammar t a -> Grammar t [a]
many cfg =  cons :$: cfg :*: many cfg
        :|: nil  :$: Void

some :: Grammar t a -> Grammar t [a]
some cfg =  cons :$: cfg :*: many cfg
