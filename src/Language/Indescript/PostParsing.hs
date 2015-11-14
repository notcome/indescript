{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Indent.PostParsing where

{-

  import           Data.Map (Map)
  import qualified Data.Map as M
  import           Data.Set (Set)
  import qualified Data.Set as S

  data PPEnv = PPEnv
             { boundVars  :: Map Variable ElemPos
             , freeVars   :: Map Variable ElemPos
             , opFixities :: Map Variable (Fixity ElemPos)
             , typeAnnots :: Map Variable (Type ElemPos)
             } deriving (Eq, Show)
  emptyPPEnv = PPEnv M.empty M.empty M.empty M.empty

  gatherDecls :: [Decl ElemPos] -> Decl PPEnv
  gatherDecls decls = foldr buildEnv emptyPPEnv decls
    where
      insertBoundVar :: Map Variable ElemPos -> (Variable, ElemPos) -> m (Map Variable ElemPos)
      insertBoundVar env (var, pos) = case M.lookup var env of
        Just pos' -> throwError $ errorBoundVarRedefined var pos' pos
        Nothing   -> return $ M.insert var pos env

      buildEnv (DeclFn lhs _ _ pos) env
        | FnArgs (Var var) = insertBoundVar bound

  foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b Source
  Monadic fold over the elements of a structure, associating to the right, i.e. from right to left.



        insertLookupWithKey ::
        Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a) Source

        O(log n). Combines insert operation with old value retrieval. The expression (insertLookupWithKey f k x map) is a pair where the first element is equal to (lookup k map) and the second element equal to (insertWithKey f k x map).

      DeclFn (FnLhs a) (Expr a) [Decl a] a
                  | DeclTypeSig [Var a] (Type a) a
                  | DeclFixity  (Fixity a) a

  (a -> b -> b) -> b -> t a -> b

  {-
  (a -> b -> b) -> b -> t a -> b


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
  -}
  --
-}
