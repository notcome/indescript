module Main where

import qualified Test.Parser.SimpleCombinators as SimpComb
import qualified Test.Parser.Decl              as Decl

main = do
  SimpComb.test
  Decl.test
