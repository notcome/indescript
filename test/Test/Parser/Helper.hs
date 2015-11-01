{-# LANGUAGE TypeFamilies #-}

module Test.Parser.Helper where

import Test.HUnit
import Language.Indescript.Syntax

(==>) :: (Show err, Functor res, Eq res', Show res', res' ~ res ())
      => Either err (res x) -> res' -> Test
actual ==> expected = case actual of
  Left err -> TestCase $ assertFailure $ show err
  Right x  -> purifyAST x ~?= expected

group :: String -> [Test] -> Test
group str tests = TestLabel str $ TestList tests
