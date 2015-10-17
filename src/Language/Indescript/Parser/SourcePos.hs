{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Indescript.Parser.SourcePos where

import           Data.Convertible
import qualified Text.Megaparsec.Pos as MP

data SourcePos = SourcePos
               { sourceName   :: String
               , sourceLine   :: Int
               , sourceColumn :: Int
               -- Megaparsec's token requires the position of next token,
               -- so it is necessary to record the span of this token.
               , contentSpan  :: (Int, Int)
               } deriving (Eq, Show)

instance Convertible SourcePos MP.SourcePos where
  convert pos = MP.newPos (sourceName   pos)
                          (sourceLine   pos)
                          (sourceColumn pos)
