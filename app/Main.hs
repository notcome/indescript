module Main where

import Language.Indescript.TypeInference
import Language.Indescript.ADT

var  = EVar . var'
var' = LEVar
int  = ELit . LInt
bool = ELit . LBool

f1 = EApp
f2 f x y = (EApp (f1 f x) y)

succ' :: Exp
succ' = EAbs (var' "x") (f2 (var "add") (var "x") (int 1))

pred' :: Exp
pred' = EAbs (var' "x") (f2 (var "sub") (var "x") (int 1))

isZero :: Exp
isZero = EAbs (var' "x") (f2 (var "eq") (var "x") (int 0))

isOne :: Exp
isOne = EAbs (var' "x") (f2 (var "eq") (var "x") (int 1))

-- let sum n = if isZero n then 0 else add n (sum (n - 1)) in sum
sum' :: Exp
sum' = ELet (var' "sum") f (var "sum")
  where f = EAbs (var' "n") (EIf (f1 isZero (var "n")) (int 0) (f2 (var "add") (var "n") (f1 (var "sum") (f1 pred' (var "n")))))

main = print $ map infer [succ', pred', isZero, sum']
