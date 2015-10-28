{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Indescript.Parser.Pos where

import           Data.Monoid
import qualified Text.Megaparsec.Pos       as MP
import           Text.Megaparsec.ShowToken

type Width  = Int
type LCPos  = (Int, Int)
type LCWPos = (Int, Int, Int)

data SourcePos = SourcePos
               { sourceLine   :: Int
               , sourceColumn :: Int
               , contentWidth :: Int
               , nextElemPos  :: SourcePos
               } deriving (Eq, Show)

fromLCWPos (l, c, w) next = SourcePos l c w next

instance ShowToken SourcePos where
  showToken = show

instance Monoid SourcePos where
