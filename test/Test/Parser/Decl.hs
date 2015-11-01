{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind     #-}

module Test.Parser.Decl where

import Test.HUnit
import Test.Parser.Helper

import Language.Indescript.Syntax
import Language.Indescript.Parser

test = do
  putStrLn "\nTest.Parser.Decl"
  runTestTT testTypeDecl
  runTestTT testGenDecl

testTypeDecl = group "Type Decl" [simpleType, typeAlias, newtype', dataType]
  where
-- TODO: add cases that should fail.
    simpleType = group "SimpleType" [
        ass "Maybe a"    ==> maybeA
      , ass "Either a b" ==> eitherAB
      , ass "a :+: b"    ==> aPlusB
      , ass "(:+:) a"    ==> plusA
      ] where ass = testParse pSimpleType

    tconid t = TCon (ConId t) ()
    tvarid v = TVar (VarId v) ()

    int     = tconid "Int"
    maybe'  = tconid "Maybe"
    either' = tconid "Either"
    plus    = TCon (ConSym ":+:") ()
    plusOp  = Op (ConSym ":+:") ()
    arrow'  = Op (ConSym "->") ()

    tvA = tvarid "a"
    tvB = tvarid "b"

    maybeA   = TApp maybe' [tvA] ()
    maybeInt = TApp maybe' [int] ()
    eitherAB = TApp either' [tvA, tvB] ()
    aPlusB   = TInfix tvA plusOp tvB ()
    plusA    = TApp plus [tvA] ()

    typeAlias = group "Type Alias" [
        ass (mkTA "Maybe" "Maybe Int")      ==> ta1
      , ass (mkTA "Maybe a" "Either Int a") ==> ta2
      ] where ass = testParse pTypeAlias

    mkTA l r = "type " ++ l ++ " = " ++ r
    ta1 = DeclTypeAlias maybe' maybeInt ()
    ta2 = DeclTypeAlias maybeA rhs ()
      where rhs = TApp either' [int, tvA] ()

    newtype' = group "Newtype" [
        ass (mkNT "Maybe" "Maybe Int")          ==> nt1
      , ass (mkNT "Maybe a" "Either (Maybe a)") ==> nt2
      ] where ass = testParse pNewType

    mkNT l r = "newtype " ++ l ++ " = " ++ r
    nt1 = DeclNewType maybe' maybe' int ()
    nt2 = DeclNewType maybeA either' maybeA ()

    dataType  = group "Algebra Data Type" [
        ass "data Maybe a = Just a | Nothing"
          ==> adt1
      , ass "data Tree a = Leaf a | Branch (Tree a) (Tree a)"
          ==> adt2
      , ass "data Tree a = Tree a :+: Tree a | Branch (Tree a) (Tree a)"
          ==> adt3
      ] where ass = testParse pDataType

    wrapper tc tv = TApp (tconid tc) [tvarid tv] ()
    treeA    = wrapper "Tree" "a"
    branchTT = TApp (tconid "Branch") [treeA, treeA] ()
    tPlusT   = TInfix treeA plusOp treeA ()

    adt1 = DeclDataType maybeA [ wrapper "Just" "a"
                               , TCon (ConId "Nothing") ()] ()
    adt2 = DeclDataType treeA [ wrapper "Leaf" "a", branchTT ] ()
    adt3 = DeclDataType treeA [ tPlusT, branchTT ] ()

testGenDecl = group "Gen Decl" [fixity, typeSig]
  where
    fixity = group "Fixity Decl" [
        ass "infixl +"        ==> fix1
      , ass "infixl :+:"      ==> fix2
      , ass "infixl 3 :+:, +" ==> fix3
      ] where ass = testParse pGenDecl

    plus    = Op (VarSym "+") ()
    conPlus = Op (ConSym ":+:") ()

    fix1 = DeclFixity (Fixity InfixL [plus] ()) ()
    fix2 = DeclFixity (Fixity InfixL [conPlus] ()) ()
    fix3 = DeclFixity (FixityLv InfixL 3 [conPlus, plus] ()) ()

    typeSig = group "Type Signature" [
        ass "(:+:), foo, Bar :: Int" ==> sig1
      ] where ass = testParse pGenDecl

    sig1 = DeclTypeSig vars (TCon (ConId "Int") ()) ()
      where vars = [ Var (ConSym ":+:") ()
                   , Var (VarId "foo") ()
                   , Var (ConId "Bar") ()
                   ]
