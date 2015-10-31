{-# LANGUAGE FlexibleContexts #-}
module Language.Indescript.Parser.Test where

import Control.Monad.State
import qualified Text.Megaparsec as MP

import Language.Indescript.Parser
import Language.Indescript.Parser.Pos
import Language.Indescript.Parser.Lexer
import Language.Indescript.Parser.Prim
import Language.Indescript.Syntax

(==>) = (,)

assert :: (Functor a, Eq (a ()), Show (a ())) =>
       MP.ParsecT [PosedToken] (State ElemPos) (a ElemPos)
       -> (String, () -> a ()) -> IO ()
assert parser (input, output) = showRes runTest
  where
    showRes :: Either String () -> IO ()
    showRes (Left x) = do
      putStrLn "Error!"
      putStrLn "Input:"
      putStrLn input
      putStrLn "Error Info"
      putStrLn x
    showRes _        = return ()

    runTest = case testParse parser input of
      Left err -> Left $ show err
      Right x  -> if purifyAST x == output ()
                  then Right ()
                  else Left $ "Desired:\n" ++ show (output ())++ "\n" ++
                              "Actual:\n" ++ show (purifyAST x) ++ "\n"

testAExprSimple = do
  ass $ "foo"  ==> EVar (VarId "foo")
  ass $ "Bar"  ==> ECon (ConId "Bar")
  ass $ "(+)"  ==> EVar (VarSym "+")
  ass $ "(:+)" ==> ECon (ConSym ":+")
  ass $ "()"   ==> ECon (ConSym "()")
  ass $ "[]"   ==> ECon (ConSym "[]")
  ass $ "(,)"  ==> ECon (ConSym "(,)")
  where ass = assert pAExpr
