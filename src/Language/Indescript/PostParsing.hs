{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
module Language.Indent.PostParsing where

import Language.Indescript.Parser.Pos
import Language.Indescript.Syntax

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

data Environment a = Environment
                   { annotations :: Map Variable (Type a)
                   , boundVars   :: Set Variable
                   , freeVars    :: Set Variable
                   , opDecls     :: Map Variable (Fixity a)
                   }

--merge :: Environment a -> Environment a
impossible = error "Confident Impossbility!"

gatherEnv :: Environment a -> m (Environment a)
gatherEnv decls = do
  let funcs  = concatMap gatherFunc   decls
  let annots = concatMap gatherAnnot  decls
  let fixity = concatMap gatherFixity decls
  boundVars'   <- buildBoundVars   funcs
  annotations' <- buildAnnotations annots
  opDecls      <- buildOpDecls     fixity
  return $ Environment boundVars' annotations S.empty opDecls
  where
    gatherFunc   (DeclFn lhs _ _ pos)
      | FnArgs (Var var _) _ _  <- lhs = [(var, pos)]
      | FnOp _ (Op op _) _ _    <- lhs = [(op, pos)]
    gatherFunc   _ = []
    gatherAnnots (DeclTypeSig vars ty pos) = map build vars
      where build var = (var, (ty, pos))
    gatherAnnots _ = []
    gatherFixity (DeclFixity x pos)

findFuncs  (DeclFn lhs _ _ pos)
  | FnArgs (Var var _) _ _  <- lhs = (var, pos)
  | FnOp _ (Op op _) _ _    <- lhs = (op, pos)
findFuncs  _                  = impossible
findAnnots (DeclTypeSig vars ty pos) = map build vars
  where build var = (var, (ty, pos))
findAnnots _                         = impossible
findFixity
--
-- postParse :: forall a. [Decl a] -> [Decl (Environment a, a)]
-- postParse =
--   where
--     processDecls :: [Decl a] -> [Decl (Environment a, a)]
--     processDecls decls = let
--       blockAnnots = filter (\case decls ->
--         DeclTypeSig vars ty _ ->
--           envWithAnnots $ M.fromList $ zip vars $ repeat ty
--         _
--       )
