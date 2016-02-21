{-# LANGUAGE TemplateHaskell #-}

module Control.IxPF.TH where

import Language.Haskell.TH

deriveIxPFType :: Name -> Q [Dec]
deriveIxPFType input = do
  TyConI dec <- reify input
  (name, ctx, tvs, cons, drvs) <- extractDecInfo dec
  (ixPFName, fname, conTy) <- makeIxPFCon name
  let cons' = map (replaceCon name conTy) cons
  let tvs'  = ixPFTvs fname tvs
  return [ DataD ctx ixPFName tvs' cons' drvs
         , TySynInstD (mkName "IxPF") $ TySynEqn [ConT name] (ConT ixPFName)
         ]

extractDecInfo :: Dec -> Q (Name, Cxt, [TyVarBndr], [Con], [Name])
extractDecInfo (DataD ctx name tvs cons drvs)
  = return (name, ctx, tvs, cons, drvs)
extractDecInfo _
  = fail "extractDecInfo: expect a declartion of data type."

ixPFTvs :: Name -> [TyVarBndr] -> [TyVarBndr]
ixPFTvs fname [ixTv] = let
  ixTvKind = case ixTv of
    PlainTV _    -> StarT
    KindedTV _ k -> k
  fTv      = KindedTV fname (AppT (AppT ArrowT ixTvKind) StarT)
  in [fTv, ixTv]
ixPFTvs _     _      = fail "ixPFTvs: input ADT should have kind k -> *"

makeIxPFCon :: Name -> Q (Name, Name, Type)
makeIxPFCon name = do
  let ixPFName = mkName $ (nameBase name) ++ "F"
  fname <- newName "f"
  conTy <- varT fname
  return (ixPFName, fname, conTy)

replaceCon :: Name -> Type -> Con -> Con
replaceCon oldName newConT = replaceC where
  oldConT = ConT oldName

  renameField name = mkName $ (nameBase name) ++ "F"
  renameInfix name = mkName $ (nameBase name) ++ "/"

  replaceC (NormalC n ts)   = NormalC (renameField n)
                                [(x, replaceT t) | (x, t) <- ts]
  replaceC (RecC n0 ts)     = RecC (renameField n0)
                                [(n, x, replaceT t) | (n, x, t) <- ts]
  replaceC (InfixC t1 o t2) = InfixC (replaceT <$> t1)
                                     (renameInfix o)
                                     (replaceT <$> t2)
  replaceC (ForallC tvs ctx con) = ForallC tvs ctx $ replaceC con

  replaceT (ForallT tvs ctx t) = ForallT tvs ctx $ replaceT t
  replaceT (AppT tf tx)        = AppT (replaceT tf) (replaceT tx)
  replaceT (SigT t k)          = SigT (replaceT t) k
  replaceT t@(VarT _)          = t
  replaceT t@(ConT _)
    | t == oldConT             = newConT
    | otherwise                = t
  replaceT t                   = t

deriveIxPFTraversal :: Name -> Q [Dec]
deriveIxPFTraversal input = do
  TyConI dec <- reify input
  (name, _, tvs, cons, _) <- extractDecInfo dec
  let tv    = case tvs of
                (x:_) -> x
                _     -> error "should have exactly 2 tvs"
  let fname = case tv of
                KindedTV n _ -> n
                _            -> error "should be a KindedTV"
  itraverseF <- traversalFun fname cons
  let imapF = FunD (mkName "imap")
                   [Clause [] (NormalB (VarE (mkName "imapDefault"))) []]
  let traverseI = InstanceD [] (AppT (ConT $ mkName "IxTraversable")
                                     (ConT name)) [itraverseF]
  let functorI  = InstanceD [] (AppT (ConT $ mkName "IxFunctor")
                                     (ConT name)) [imapF]
  return [traverseI, functorI]

traversalFun :: Name -> [Con] -> Q Dec
traversalFun fname cons = do
  funN <- newName "f"
  varN <- newName "x"
  let traversalCase = CaseE (VarE varN)
                            (traversalBranches (VarT fname) funN cons)
  let fclause = Clause [VarP funN, VarP varN] (NormalB traversalCase) []
  return $ FunD (mkName "itraverse") [fclause]

traversalBranches :: Type -> Name -> [Con] -> [Match]
traversalBranches ftype funN cons = map traversalBranch cons where
  vnameSeq = [ mkName ('v' : show i)| i <- [1..] :: [Int]]

  pureE    e = AppE (VarE $ mkName "pure") e
  funE     e = AppE (VarE funN) e
  traveseE e = AppE (AppE (VarE $ mkName "traverse") (VarE funN)) e

  traversalBranch (ForallC _ _ con)  = traversalBranch con
  traversalBranch (NormalC n sts)    = let
    fields = [t | (_, t) <- sts]
    in matchFromFields n fields
  traversalBranch (RecC n vsts)      = let
    fields = [t | (_, _, t) <- vsts]
    in matchFromFields n fields
  traversalBranch (InfixC st1 o st2) = matchFromInfix (snd st1) o (snd st2)

  matchFromFields n ts   = let
    values = [valueFromField vn vt | (vn, vt) <- zip vnameSeq ts]
    body   = NormalB $ expFromExps (ConE n) values
    in Match (patFromFields n ts)    body []
  matchFromInfix t1 o t2 = let
    values = [valueFromField vn vt | (vn, vt) <- zip vnameSeq [t1, t2]]
    body   = NormalB $ expFromExps (ParensE (VarE o)) values
    in Match (patFromInfix t1 o t2)  body []

  patFromFields n ts = ConP n [VarP vn | vn <- take (length ts) vnameSeq]
  patFromInfix _ o _ = InfixP (VarP $ vnameSeq !! 0) o
                              (VarP $ vnameSeq !! 1)

  expFromExps base []     = pureE base
  expFromExps base (v:vs) = foldl star dollar vs where
    star  v1 v2 = UInfixE v1   (VarE $ mkName "<*>") v2
    dollar      = UInfixE base (VarE $ mkName "<$>") v

  valueFromField :: Name -> Type -> Exp
  valueFromField vname (AppT ftype' _)
    | ftype == ftype'    = funE     (VarE vname)
  valueFromField vname (AppT _ (AppT ftype' _))
    | ftype == ftype'    = traveseE (VarE vname)
  valueFromField vname _ = pureE    (VarE vname)
