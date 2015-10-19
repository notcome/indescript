{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Indescript.Parser.SourcePos where

import           Data.Convertible
import qualified Text.Megaparsec.Pos       as MP
import           Text.Megaparsec.ShowToken

data SourcePos = SourcePos
               { sourceName   :: String
               , sourceLine   :: Int
               , sourceColumn :: Int
               -- Megaparsec's token requires the position of next token,
               -- so it is necessary to record the span of this token.
               , contentSpan  :: (Int, Int)
               } deriving (Eq, Show)

instance ShowToken SourcePos where
  showToken = show

advanceSourcePos :: SourcePos -> SourcePos
advanceSourcePos pos@(SourcePos _ line column (dl, dc)) =
  pos { sourceLine   = line   + dl
      , sourceColumn = column + dc
      , contentSpan  = (0, 0) }

instance Convertible SourcePos MP.SourcePos where
  convert pos = MP.newPos (sourceName   pos)
                          (sourceLine   pos)
                          (sourceColumn pos)
