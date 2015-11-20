-- Poster refers to post-parsing processes. It performs several functions:
--   1. Check certian invalid occurences of tokens, like normal operators inside
--      patterns.
--   2. Adjust AST according to operator fixity information.
--   3. Check redefinitions and missing definitions, and group definitions of
--      the same functions.
--   4. Check unbound variables and build a bound name table.

{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Indescript.Parser.Poster where

import Control.Arrow
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable
import Data.List

import qualified Data.Map as M

import Control.IxFix
import Language.Indescript.AST.SourcePos
import Language.Indescript.AST

type ISParserEnv m = MonadError String m

--   ## Group Equations
groupEquations :: forall a t st i.
               ( t ~ AnnotAstF ElemPos, a ~ IxFix (AnnotAstF ElemPos)
               , st ~ M.Map Var (ElemPos, (AssocType, Int))
               ) => a i -> (a i)
groupEquations ast = ana (out >>> psi) ast where
  psi :: forall i'. t a i' -> t a i'
  psi (Annot (pos, DeclsF decls)) = let
    children = unfoldr consumeGroup (decls :: [a AstDecl])
    in Annot (pos, DeclsF children)
  psi x                           = x

  consumeGroup :: [a AstDecl] -> Maybe (a AstDecl, [a AstDecl])
  consumeGroup []     = Nothing
  consumeGroup (d:ds) = case getFnName d of
      Nothing   -> Just (d, ds)
      Just name -> let
        (this, rest) = break ((== Just name) . getFnName) (d:ds)
        positions    = map (fst . unAnnot . out) this
        grouped      = In $ Annot (elemPos positions, FnDefF name this)
        in Just (grouped, rest)

  getFnName :: a AstDecl -> Maybe Var
  getFnName = out >>> unAnnot >>> snd >>> (\case
    EqtF lhs _ _ -> let
      name  = case lhs of FnArgsF x _ -> x
                          FnOpF _ x _ -> x
      (VarF name' _) = snd $ unAnnot $ out name
      in Just name'
    _            -> Nothing)

--   ## Adjust AST
adjustAst :: forall m0 m a t st i.
          ( ISParserEnv m0, m ~ StateT st m0
          , t ~ AnnotAstF ElemPos, a ~ IxFix (AnnotAstF ElemPos)
          , st ~ M.Map Var (ElemPos, (AssocType, Int))
          ) => a i -> m0 (a i)
adjustAst ast = evalStateT (anaM psi $ ast) M.empty where
  psi :: forall i'. a i' -> m (t a i')
  psi (In x@(Annot (_, node))) = do
    () <- case node of
      LetF ds _    -> collect (out ds)  >>= mergeEnv >>= put
      EqtF _ _ whs -> collect (out whs) >>= mergeEnv >>= put
      DeclsF _     -> collect x         >>= mergeEnv >>= put
      _            -> return ()
    case node of
      InfixF _ _ _ -> adjust x
      _            -> return x

  mergeEnv :: st -> m st
  mergeEnv news = M.union <$> pure news <*> get

  collect :: t a AstDecl -> m st
  collect x = let
    (DeclsF xs) = snd $ unAnnot x

    unOp (VarF op _) = op
    unOp (ConF op _) = op
    unOp _           = impossible

    fixities = concat $ do
      let xs' = map (unAnnot . out) xs
      (pos, (OpFixF dir lv ops)) <- xs'
      let info = (pos, (dir, lv))
      let ops' = map (snd . unAnnot . out) ops
      return [(unOp op, info) | op <- ops']
    build (op, v@(pos', _)) env =
      case M.lookup op env of
        Just (pos, _) -> throwError $ errorRedefineOpFixity op pos pos'
        Nothing       -> return $ M.insert op v env
    in foldrM build M.empty fixities

  adjust :: forall i'. t a i' -> m (t a i')
  adjust x = do
    env <- get
    case resolveAssoc (findWithDefault env 9 $ snd . snd)
                      (findWithDefault env Infix $ fst . snd)
                      x of
      Left errMsg -> throwError errMsg
      Right x'    -> return x'
    where
      findWithDefault env def f = \k ->
        case M.lookup k env of
          Just i  -> f i
          Nothing -> def

--  ### resolveAssoc
-- A direct translation from the one specified in Haskell Language Report 2010.
-- It’s put at top-level for test.
resolveAssoc :: forall ast dop i.
             ( ast ~ AnnotAstF ElemPos (IxFix (AnnotAstF ElemPos)) i
             , dop ~ (Var, Var -> IxFix (AnnotAstF ElemPos) i))
             => (Var -> Int) -> (Var -> AssocType)
             -> ast -> Either String ast
resolveAssoc lv assoc ast = case getFullExpr ast of
  (Left o1 : Right e1 : rest)
    -> case parse o1 e1 rest of
      Just (x, _) -> Right x
      Nothing     -> Left "failure"
  _ -> Right ast
  where
    getFullExpr :: ast -> [Either dop ast]
    getFullExpr (Annot (_, InfixF l o r)) =
      Right (out l) : Left ((unOp . snd . unAnnot . out) o, const o)
                    : getFullExpr (out r)
      where unOp (VarF op _) = op
            unOp (ConF op _) = op
            unOp _           = impossible
    getFullExpr _ = []

    parse :: dop -> ast -> [Either dop ast] -> Maybe (ast, [Either dop ast])
    parse _ e1 [] = Just (e1, [])
    parse (o1, f1) e1 (Left (o2, f2) : Right e2 : rest)
      | lv o1 == lv o2 && (assoc o1 /= assoc o2 || assoc o1 == Infix)
      = Nothing
      | lv o1 > lv o2 || (lv o1 == lv o2 && assoc o1 == InfixL)
      = Just (e1, Left (o2, f2) : Right e2 : rest)
      | otherwise
      = do (e2', rest') <- parse (o2, f2) e2 rest
           let bareApp  =  AppF (f2 o2) [In e1, In e2']
           let (p1, p2) =  (fst $ unAnnot e1, fst $ unAnnot e2')
           let combined =  Annot (elemPos (p1, p2), bareApp)
           parse (o1, f1) combined rest'
    parse _ _ _ = impossible

errorRedefineOpFixity :: Var -> ElemPos -> ElemPos -> String
errorRedefineOpFixity op pos pos' = "redefine op " ++
  show op ++ " at pos " ++ show pos ++ " and " ++ show pos'

impossible :: a
impossible = error "impossible!"
