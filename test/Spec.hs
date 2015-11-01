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
  runTestTT testPat
  runTestTT testType
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

testPat = group "Pat" [simple, complex]
  where
    simple = group "Simple Pattern"
      [apatSimp, fpatSimp, lpatSimp, patSimp]
    complex = group "Complex Pattern" cmpxExpr

    apatSimp = group "APat Simple" [
        ass "foo"  ==> foo
      , ass "Bar"  ==> _bar
      , ass "3.14" ==> pi_
      , ass "_"    ==> PWildcard ()
      , ass "()"   ==> unit
      , ass "[]"   ==> list
      , ass "(:+)" ==> plus
      ] where ass = testParse pAPat

    foo  = PVar (VarId "foo") ()
    _bar = PCon (ConId "Bar") ()
    pi_  = PLit (LFloat 3.14) ()
    unit = PCon (ConSym "()") ()
    list = PCon (ConSym "[]") ()
    plus = PCon (ConSym ":+") ()

    fpatSimp = group "FPat Simple" [
        ass "Bar foo 3.14"  ==> _bar_foo_pi
      , ass "(:+) foo 3.14" ==> plus_foo_pi
      ] where ass = testParse pFPat

    _bar_foo_pi = PApp _bar [foo, pi_] ()
    plus_foo_pi = PApp plus [foo, pi_] ()

    lpatSimp = group "LPat Simple" [
        ass "foo"           ==> foo
      , ass "Bar foo 3.14"  ==> _bar_foo_pi
      , ass "(:+) foo 3.14" ==> plus_foo_pi
      , ass "-999"          ==> neg_999
      , ass "-3.14"         ==> neg_pi
      ] where ass = testParse pLPat

    neg_999 = PLit (LInt (-999)) ()
    neg_pi  = PLit (LFloat (-3.14)) ()

    patSimp = group "Pat Simple" [
        ass "-999"         ==> neg_999
      , ass "3.14"         ==> pi_
      , ass "foo : []"     ==> list_foo
      , ass "-999 :+ 3.14" ==> _999_plus_pi
      ] where ass = testParse pPat

    opPlus       = Op (ConSym ":+") ()
    opCons       = Op (ConSym ":") ()
    list_foo     = PInfix foo opCons list ()
    _999_plus_pi = PInfix neg_999 opPlus pi_ ()

    cmpxExpr = [
        ass "foo@(-999)" ==> neg_999_as_foo
      , ass sCmpx1       ==> list_2foo
      , ass sCmpx2       ==> _bar_2pi_as_foo
      , ass sCmpx3       ==> cmpx3
      ] where ass = testParse pPat

    sCmpx1 = "foo:foo:[]"
    sCmpx2 = "foo@(Bar 3.14 3.14)"
    sCmpx3 = "foo@((" ++ sCmpx1 ++ ") :+ " ++ sCmpx2 ++ ")"

    neg_999_as_foo  = PAs foo neg_999 ()
    list_2foo       = PInfix foo opCons list_foo ()
    _bar_2pi_as_foo = PAs foo rhs ()
      where rhs = PApp _bar [pi_, pi_] ()
    cmpx3           = PAs foo rhs ()
      where rhs = PInfix list_2foo opPlus _bar_2pi_as_foo ()

testType = group "Type" [simple, complex]
  where
    simple = group "Simple Type"
      [atypeSimp, ftypeSimp, typeSimp]
    complex = group "Complex Type" cmpxType

    atypeSimp = group "AType Simple" [
        ass "m"    ==> monad
      , ass "Int"  ==> int
      , ass "()"   ==> unit
      , ass "[]"   ==> list
      , ass "(,)"  ==> tuple
      , ass "(->)" ==> arrow
      ] where ass = testParse pAType

    monad = TVar (VarId "m") ()
    int   = TCon (ConId "Int") ()
    unit  = TCon (ConSym "()") ()
    list  = TCon (ConSym "[]") ()
    tuple = TCon (ConSym "(,)") ()
    arrow = TCon (ConSym "->") ()

    ftypeSimp = group "FType Simple" [
        ass "m"      ==> monad
      , ass "m Int"  ==> mInt
      , ass "[] Int" ==> listInt
      , ass sI2I     ==> int2Int
      ] where ass = testParse pFType

    sI2I    = "(->) Int Int"
    mInt    = TApp monad [int] ()
    listInt = TApp list [int] ()
    int2Int = TApp arrow [int, int] ()

    typeSimp = group "Type Simple" [
        ass "Int"   ==> int
      , ass "(Int)" ==> int
      , ass sI2I    ==> int2Int
      , ass sI2I'   ==> int2Int'
      , ass sEither ==> either2Int
      , ass sPlus   ==> plus2Int
      , ass sForall ==> forall
      ] where ass = testParse pType

    sI2I'    = "Int -> Int"
    sEither  = "Int `Either` Int"
    sPlus    = "Int :+: Int"
    sForall  = "forall m. m Int"

    tconPlus   = Op (ConSym ":+:") ()
    tconEither = Op (ConId "Either") ()
    arrowOp    = Op (ConSym "->") ()
    int2Int'   = TInfix int arrowOp int ()
    either2Int = TInfix int tconEither int ()
    plus2Int   = TInfix int tconPlus int ()
    forall     = TForall [monad] mInt ()

    cmpxType = [
        ass sCmpx1   ==> cmpx1
      , ass sCmpx1'  ==> cmpx1
      , ass sCmpx1'' ==> cmpx1
      , ass "(Int)"  ==> int
      , ass sCmpx2   ==> cmpx2
      ] where ass = testParse pType

    sCmpx1   = "Int -> Int -> Int"
    sCmpx1'  = "Int -> (Int -> Int)"
    sCmpx1'' = "(Int -> Int -> Int)"
    sCmpx2   = "m Int -> m ()"

    cmpx1 = TInfix int arrowOp int2Int' ()
    cmpx2 = TInfix mInt arrowOp mUnit ()
      where mUnit = TApp monad [unit] ()
