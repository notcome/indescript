{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Convertible where

class Convertible a b where
  convert :: a -> b
