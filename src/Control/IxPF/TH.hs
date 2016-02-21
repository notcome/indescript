{-# LANGUAGE TemplateHaskell #-}

module Control.IxPF.TH where

import Language.Haskell.TH

deriveIxPF :: Name -> Q [Dec]
deriveIxPF input = do
  TyConI dec <- reify input
  (name, ctx, tvs, cons, drvs) <- extractDecInfo dec
  (ixPFName, fname, conTy) <- makeIxPFCon name
  let cons' = map (replaceCon name conTy) cons
  let tvs'  = ixPFTvs fname tvs
  return [DataD ctx ixPFName tvs' cons' drvs]

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
  conTy <- appT (conT ixPFName) (varT fname)
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
