{-# LANGUAGE TemplateHaskell #-}

module Control.SemiIso.TH where

import Language.Haskell.TH

import Control.SemiIso (SemiIso (SemiIso))

deriveSemiIsos :: Name -> Q [Dec]
deriveSemiIsos input = do
  TyConI dec <- reify input
  (name, tvs, cons) <- extractDecInfo dec
  let defineSemiIso' = defineSemiIso name tvs $ defaultBranch cons
  sequence $ concatMap defineSemiIso' cons

extractDecInfo :: Dec -> Q (Name, [TyVarBndr], [Con])
extractDecInfo (DataD    _ name tvs cons _) = return (name, tvs, cons)
extractDecInfo (NewtypeD _ name tvs con _)  = return (name, tvs, [con])
extractDecInfo _                       = fail
  "extractDecInfo: expect a declaration of data type or newtype."

buildDecType :: Name -> [TyVarBndr] -> Q Type
buildDecType name tvs = foldl appT con tvs'
  where
    tvname (PlainTV n)    = n
    tvname (KindedTV n _) = n

    tvs' = [varT (tvname tv) | tv <- tvs]
    con  = conT name

constructorName :: Con -> Name
constructorName (NormalC name _)   = name
constructorName (RecC    name _)   = name
constructorName (InfixC  _ name _) = name
constructorName (ForallC _ _ con)  = constructorName con

constructorFields :: Con -> [Type]
constructorFields (NormalC _ fields) = [ty | (_, ty)    <- fields  ]
constructorFields (RecC    _ fields) = [ty | (_, _, ty) <- fields  ]
constructorFields (InfixC f1 _ f2)   = [ty | (_, ty)    <- [f1, f2]]
constructorFields (ForallC _ _ con)  = constructorFields con

constructorSize :: Con -> Int
constructorSize = length . constructorFields

constructorTypes :: Con -> ([TyVarBndr], Cxt, [Type])
constructorTypes con@(NormalC _ _)     = ([], [],   constructorFields con)
constructorTypes con@(RecC    _ _)     = ([], [],   constructorFields con)
constructorTypes con@(InfixC _ _ _)    = ([], [],   constructorFields con)
constructorTypes (ForallC tvs ctx con) = (tvs, ctx, constructorFields con)

isoName :: Name -> Name
isoName qname = let name = nameBase qname in mkName ('_' : name)

defaultBranch :: [Con] -> [Q Clause]
defaultBranch cons
  | length cons > 1 = [clause [wildP] (normalB [| Nothing |]) []]
  | otherwise       = []

defineSemiIso :: Name -> [TyVarBndr] -> [Q Clause] -> Con -> [Q Dec]
defineSemiIso decName tvs defaultB con = let
  name = isoName $ conName
  body = clause [] (normalB [| SemiIso $(inE) $(outE)|]) [in', outDec, out]
  in'  = funD (mkName "in'") [inB]
  out  = funD (mkName "out") (outB : defaultB)
  inE  = varE (mkName "in'")
  outE = varE (mkName "out")
  inB  = clause [uncurried tupP ps]
                (normalB $ foldl appE (conE conName) vs)
                []
  outB = clause [conP conName ps]
                (normalB [| Just $(uncurried tupE vs) |])
                []

  tupType = uncurried tupT fts'
  isoType = forallT (tvs ++ conTvs) (return conCxt)
                    (appT (appT (conT $ mkName "SemiIso") tupType) decType)
  outType = let
    fromType = decType
    toType   = (appT (conT $ mkName "Maybe") tupType)
    in forallT (tvs ++ conTvs) (return conCxt)
             (appT (appT arrowT fromType) toType)

  outDec = sigD (mkName "out")  outType
  in [sigD name isoType, funD name [body]] where
    conName  = constructorName con
    conSize  = constructorSize con
    (vs, ps) = let vnames = [mkName ("v" ++ show i) | i <- [1..conSize]]
               in (map varE vnames, map varP vnames)
    decType  = buildDecType decName tvs

    (conTvs, conCxt, fts) = constructorTypes con
    fts'                  = map return fts

    tupT [t1, t2] = appT (appT (tupleT 2) t1) t2
    tupT _        = error "tupT only accepts two TypeQ"

    uncurried tupX []     = tupX []
    uncurried _    [x]    = x
    uncurried tupX (x:xs) = tupX [x, uncurried tupX xs]
