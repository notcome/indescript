module Main where

import Test.HUnit

import Language.Indescript.Syntax
import Language.Indescript.Parser

actual ==> expected = case actual of
  Left err -> TestCase $ assertFailure $ show err
  Right x  -> purifyAST x ~?= expected

group str tests = TestLabel str $ TestList tests

main :: IO ()
main = do
  runTestTT testExpr
  return ()

testExpr = group "Expr" [simple, complex]
  where
    simple  = group "Simple Expression"
      [aexprSimp, fexprSimp, lexprSimp, exprSimp]
    complex = group "Complex Expression" cmpxExpr

    aexprSimp = group "AExpr Simple" [
        ass "foo"   ==> foo
      , ass "Bar"   ==> _bar
      , ass "(+)"   ==> plus
      , ass "(:+:)" ==> conPlus
      , ass "()"    ==> unit
      , ass "[]"    ==> list
      , ass "(,)"   ==> tuple

      , ass "1"      ==> num1
      , ass "0b1010" ==> num10
      , ass "0x3F"   ==> num63
      , ass "999"    ==> num999
      ] where ass = testParse pAExpr

    foo     = EVar (VarId "foo")  ()
    _bar    = ECon (ConId "Bar")  ()
    plus    = EVar (VarSym "+")   ()
    conPlus = ECon (ConSym ":+:") ()
    unit    = ECon (ConSym "()")  ()
    list    = ECon (ConSym "[]")  ()
    tuple   = ECon (ConSym "(,)") ()

    num1   = ELit (LInt 1)   ()
    num10  = ELit (LInt 10)  ()
    num63  = ELit (LInt 63)  ()
    num999 = ELit (LInt 999) ()

    fexprSimp = group "FExpr Simple" [
        ass "foo"      ==> foo
      , ass "foo Bar"  ==> foo_Bar
      , ass "Bar foo"  ==> _bar_foo
      , ass "(+) 1 10" ==> plus_1_10
      , ass "(:+:) foo Bar"
                       ==> conPlus_foo_Bar
      , ass "(,) Bar 0o1747"
                       ==> tuple_Bar_999
      ] where ass = testParse pFExpr

    -- List of AST pieces for FExpr Simple
    foo_Bar         = EApp foo [_bar] ()
    _bar_foo        = EApp _bar [foo] ()
    plus_1_10       = EApp plus [num1, num10]   ()
    conPlus_foo_Bar = EApp conPlus [foo, _bar]  ()
    tuple_Bar_999   = EApp tuple [_bar, num999] ()

-- Lambda-abstraction and let-binding will not be tested until
-- pattern combinators are tested.
    lexprSimp = group "LExpr Simple" [
        ass sIfThen1  ==> ifThen1
      , ass sIfThen1' ==> ifThen1
      ] where ass = testParse pLExpr

    sIfThen1  = "if foo Bar then (+) 1 10 else (:+:) foo Bar"
    sIfThen1' = "if foo Bar\n\
                \then (+) 1 10\n\
                \else (:+:) foo Bar"
    ifThen1  = EIf foo_Bar plus_1_10 conPlus_foo_Bar ()

    exprSimp = group "Expr Simple" [
        ass "999"      ==> num999
      , ass "1 + 10"   ==> _1_plus_10
      , ass "foo : []" ==> list_foo
      , ass "1 :+: 10" ==> _1_cPlus_10
      , ass "-0o1747"  ==> neg_999
      ] where ass = testParse pExpr

    opPlus  = Op (VarSym "+") ()
    opCPlus = Op (ConSym ":+:") ()
    opCons  = Op (ConSym ":") ()

    _1_plus_10  = EInfix num1 opPlus num10 ()
    _1_cPlus_10 = EInfix num1 opCPlus num10 ()
    list_foo    = EInfix foo opCons list ()
    neg_999     = ENeg num999 ()

    cmpxExpr = [
        ass "(999)"     ==> num999
      , ass "(1 + 10)"  ==> _1_plus_10
      , ass sCmpxExpr1  ==> cmpxExpr1
      , ass sCmpxExpr2  ==> cmpxExpr2
      , ass sCmpxExpr2' ==> cmpxExpr2
      ] where ass = testParse pExpr

    sCmpxExpr1 = "(1 + 10) + (-999) :+: (foo Bar)"
    cmpxExpr1   = let rhs = EInfix neg_999 opCPlus foo_Bar ()
                  in EInfix _1_plus_10 opPlus rhs ()
    sCmpxExpr2  = "if " ++ sCmpxExpr1 ++
               " then " ++ sIfThen1'  ++
               " else " ++ "-999"
    sCmpxExpr2' = "if " ++ sCmpxExpr1 ++
              " then (" ++ sIfThen1   ++
              ") else " ++ "-999"
    cmpxExpr2   = EIf cmpxExpr1 ifThen1 neg_999 ()
