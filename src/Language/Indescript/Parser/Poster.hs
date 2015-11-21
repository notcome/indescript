-- Poster refers to post-parsing processes. It performs several functions:
--   1. Check certian invalid occurences of tokens, like normal operators inside
--      patterns.
--   2. Adjust AST according to operator fixity information.
--   3. Group equations into function definitions.
--   4. Check redefinitions.
--   5. Check redefinitions.
--   4. Check unbound variables and missing declarations;
--        build a bound name table.

{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

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

--   ## Adjust AST
adjustAst :: forall m0 m a t st i.
          ( ISParserEnv m0, m ~ StateT st m0
          , t ~ AnnotAstF ElemPos, a ~ IxFix (AnnotAstF ElemPos)
          , st ~ M.Map Var (ElemPos, (AssocType, Int))
          ) => a i -> m0 (a i)
adjustAst ast = evalStateT (anaM psi ast) M.empty where
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

    fixities = concat $ do
      let xs' = map outAnnot xs
      (pos, (OpFixF dir lv ops)) <- xs'
      let info = (pos, (dir, lv))
      let ops' = map rmAnnot ops
      return [(unOp op, info) | op <- ops']
-- Fixity redefinition chec is performed afterwards, which does not affect the
--   overall correctness: if there is no conflict, the program will be adjusted
--   correctly; if not, compilation will abort anyway.
    in return $ M.fromList fixities

  adjust :: forall i'. t a i' -> m (t a i')
  adjust x = do
    env <- get
    case resolveAssoc (findWithDefault env 9 getLv)
                      (findWithDefault env InfixL getAssoc)
                      x of
      Left errMsg -> throwError errMsg
      Right x'    -> return x'
    where
      getLv    = snd . snd
      getAssoc = fst . snd

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
      Right (out l) : Left (unOp $ rmAnnot o, const o)
                    : getFullExpr (out r)
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

--   ## Group Equations
-- This operation must be performed after adjusting AST. Consider:
-- > 1 : [] +++ 2 : []
-- > infixr 9 []
-- > infixl 8 +++
groupEquations :: forall m a t i.
               ( ISParserEnv m
               , t ~ AnnotAstF ElemPos, a ~ IxFix (AnnotAstF ElemPos)
               ) => a i -> m (a i)
groupEquations ast = return $ ana (out >>> psi) ast where
  psi :: forall i'. t a i' -> t a i'
  psi (Annot (pos, DeclsF decls)) = let
    children = unfoldr consumeGroup (decls :: [a AstDecl])
    in Annot (pos, DeclsF children)
  psi x                           = x

  consumeGroup :: [a AstDecl] -> Maybe (a AstDecl, [a AstDecl])
  consumeGroup []     = Nothing
  consumeGroup (d:ds) = case fnName d of
      Nothing   -> Just (d, ds)
      Just name -> let
        (this, rest) = break ((== Just name) . fnName) (d:ds)
        positions    = map (fst . outAnnot) this
        grouped      = In $ Annot (elemPos positions, FnDefF name this)
        in Just (grouped, rest)

  fnName :: a AstDecl -> Maybe Var
  fnName = out >>> unAnnot >>> snd >>> (\case
    EqtF lhs _ _ -> let
      name  = case lhs of FnArgsF x _ -> x
                          FnOpF _ x _ -> x
      (VarF name' _) = snd $ unAnnot $ out name
      in Just name'
    _            -> Nothing)

--   ## Check Redefinitions
checkRedefns :: forall m a t i.
             ( ISParserEnv m
             , t ~ AnnotAstF ElemPos, a ~ IxFix (AnnotAstF ElemPos)
             ) => a i -> m (a i)
checkRedefns ast = anaM (out >>> psi) ast where
  psi :: forall i'. t a i' -> m (t a i')
  psi x@(Annot (_, DeclsF decls)) = check decls *> return x
  psi x                           = return x

  check :: forall i'. [a i'] -> m ()
  check xs = let
    fixities  = filterDecls (\(OpFixF _ _ ops) ->
      map (unOp . rmAnnot) ops)
    typesigs  = filterDecls (\(TySigF fns _) ->
      [fn | VarF fn _ <- map rmAnnot fns])
    functions = filterDecls (\(FnDefF name _) -> [name])

    typecons  = filterDecls ( \case
      TyAlsF ty _   -> [unTycon ty]
      NewTyF ty _ _ -> [unTycon ty]
      DatTyF ty _   -> [unTycon ty]
      _             -> impossible )

    termcons  = let
      ofNewTys = filterDecls (\(NewTyF _ ty _) -> [unTycon ty])
      ofDatTys = do
        DatTyF _ alts <- fmap rmAnnot xs
        alt           <- alts
        return (unTycon alt, fst $ outAnnot alt)
      in ofNewTys ++ ofDatTys

    in sequence_
      [ foldrM (build errorRedefineOpFixity) M.empty fixities
      , foldrM (build errorRedefineTypeSig)  M.empty typesigs
      , foldrM (build errorRedefineFunction) M.empty functions
      , foldrM (build errorRedefineTypeCon)  M.empty typecons
      , foldrM (build errorRedefineTermCon)  M.empty termcons
      ]
    where
      build err (k, pos) acc =
        case M.lookup k acc of
          Nothing   -> return $ M.insert k pos acc
          Just pos' -> throwError $ err k pos' pos

      filterDecls getId = do
        let xs' = map outAnnot xs
        (pos, decl) <- xs'
        (, pos) <$> getId decl

      unTycon x = case rmAnnot x of
        ConF tv _    -> tv
        AppF fn _    -> unOp $ rmAnnot fn
        InfixF _ o _ -> unOp $ rmAnnot o
        _         -> impossible

--    # Helper Types and Functions
unOp :: AstF f i -> Var
unOp (VarF op _) = op
unOp (ConF op _) = op
unOp _           = impossible

outAnnot :: IxFix (AnnotAstF a) i -> (a, AstF (IxFix (AnnotAstF a)) i)
outAnnot = unAnnot . out

rmAnnot :: IxFix (AnnotAstF a) i -> AstF (IxFix (AnnotAstF a)) i
rmAnnot = snd . outAnnot

errorRedefineOpFixity :: Var -> ElemPos -> ElemPos -> String
errorRedefineOpFixity op pos pos' = "redefine op " ++
  show op ++ " at pos " ++ show pos ++ " and " ++ show pos'

errorRedefineTypeSig :: Var -> ElemPos -> ElemPos -> String
errorRedefineTypeSig op pos pos' = "redefine type signature " ++
  show op ++ " at pos " ++ show pos ++ " and " ++ show pos'

errorRedefineFunction :: Var -> ElemPos -> ElemPos -> String
errorRedefineFunction fn pos pos' = "redefine function " ++
  show fn ++ " at pos " ++ show pos ++ " and " ++ show pos'

errorRedefineTypeCon :: Var -> ElemPos -> ElemPos -> String
errorRedefineTypeCon fn pos pos' = "redefine type constructor " ++
  show fn ++ " at pos " ++ show pos ++ " and " ++ show pos'

errorRedefineTermCon :: Var -> ElemPos -> ElemPos -> String
errorRedefineTermCon fn pos pos' = "redefine term constructor " ++
  show fn ++ " at pos " ++ show pos ++ " and " ++ show pos'

impossible :: a
impossible = error "impossible!"
