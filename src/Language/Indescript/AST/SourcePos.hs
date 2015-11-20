module Language.Indescript.AST.SourcePos where

--    # Datatypes and Functions
--   ## SourcePoint
newtype SourcePoint = SourcePoint { unSourcePoint :: (Int, Int) }
                    deriving (Eq, Ord)

pesudoPoint :: SourcePoint
pesudoPoint = SourcePoint (-1, -1)

--  ### Accessors
srcRow :: SourcePoint -> Int
srcRow = fst . unSourcePoint
srcCol :: SourcePoint -> Int
srcCol = snd . unSourcePoint

incCol :: Int -> SourcePoint -> SourcePoint
incCol inc p = SourcePoint (srcRow p, inc + srcCol p)
incRow :: Int -> SourcePoint -> SourcePoint
incRow inc p = SourcePoint (inc + srcRow p, srcCol p)

--   ## SourceSpan
newtype SourceSpan  = SourceSpan  { unSourceSpan  :: (Int, Int) }
                    deriving (Eq, Ord, Show)

zeroSpan :: SourceSpan
zeroSpan = SourceSpan (0, 0)

--  ### Accessors
deltaRow :: SourceSpan  -> Int
deltaRow = fst . unSourceSpan
deltaCol :: SourceSpan  -> Int
deltaCol = snd . unSourceSpan

--   ## ElemPos
data ElemPos = ElemPos
             { startPoint :: SourcePoint
             , elemSpan   :: SourceSpan
-- TODO: implement a better show instance.
             } deriving Eq

--  ### Accessors
endPoint :: ElemPos -> SourcePoint
endPoint (ElemPos p d) = let
  (r0, c0) = unSourcePoint p
  (dr, dc) = unSourceSpan  d
  in SourcePoint (r0 + dr, c0 + dc)

startRow :: ElemPos -> Int
startRow = srcRow . startPoint
startCol :: ElemPos -> Int
startCol = srcCol . startPoint

--   ## Functions
nullPos :: SourcePoint -> ElemPos
nullPos = flip ElemPos zeroSpan

diffPoint :: SourcePoint -> SourcePoint -> SourceSpan
diffPoint p0 p1 = let
  dr = srcRow p1 - srcRow p0
  dc = srcCol p1 - srcCol p0
  in SourceSpan (dr, dc)

class GetElemPos a where
  elemPos :: a -> ElemPos

instance GetElemPos ElemPos where
  elemPos = id

instance (GetElemPos a, GetElemPos b) => GetElemPos (a, b) where
  elemPos (l, r) = let
    lp = elemPos l; ls = startPoint lp; le = endPoint lp
    rp = elemPos r; rs = startPoint rp; re = endPoint rp
    ps = min ls rs
    pe = max le re
    in ElemPos ps (diffPoint ps pe)

instance GetElemPos a => GetElemPos [a] where
  elemPos = foldl1 combine . map elemPos
    where
      combine l r = elemPos (l, r)

--   ## Show Instance
instance Show SourcePoint where
  show point = "l." ++ show (srcRow point) ++
              " c." ++ show (srcCol point)

instance Show ElemPos where
  show pos = let sp = startPoint pos
                 ep = endPoint   pos
             in if srcRow sp == srcRow ep
                then show (startPoint pos) ++ " - " ++ show (srcCol ep)
                else show (startPoint pos) ++ " - " ++ show (endPoint pos)
