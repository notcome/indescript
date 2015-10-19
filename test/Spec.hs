module Main where

import Language.Indescript.Parser

main :: IO ()
main = do
  print $ parse "tc01" "(Just a)"
  print $ parse "tc02" "case Just a of { Just x -> x + 1; Nothing -> 0 }"
