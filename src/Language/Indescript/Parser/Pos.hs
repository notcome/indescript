module Language.Indescript.Parser.Pos where

--    # Datatypes and Functions
--   ## SourcePoint
newtype SourcePoint = SourcePoint { unSourcePoint :: (Int, Int) }
                    deriving (Eq, Ord, Show)

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
             } deriving (Eq, Show)

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
