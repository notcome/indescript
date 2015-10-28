{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE TupleSections #-}

module Language.Indescript.Parser.Lexer where

import Data.Char              (ord)
import Text.Regex.Applicative

import Language.Indescript.Syntax
import Language.Indescript.Parser.Pos

--    # Charsets
--   ## Number Charsets
binit = ['0'..'1']
octit = ['0'..'7']
digit = ['0'..'9']
hexit = digit ++ ['A'..'F'] ++ ['a'..'f']

cBinit = symClass binit
cOctit = symClass octit
cDigit = symClass digit
cHexit = symClass hexit

--   ## Identifier Charsets
asciiSmall = ['a'..'z'] ++ ['_']
asciiLarge = ['A'..'Z']

cSmall  = symClass asciiSmall
cLarge  = symClass asciiLarge
cIdChar = cSmall <|> cLarge <|> cDigit <|> sym '\''

--   ## Sepcial Charcters
special = "(),;[]`{}"

cSpecial = symClass special

--   ## Symbol Charsets
asciiSym = "!#$%&*+./<=>?@\\^|-~:"

cSymbol = symClass asciiSym

--   ## White Charsets
cTab   = sym '\t'
cSpace = sym ' '
cWhite = cTab <|> cSpace

cLine = symClass "\n\r\f"

--    # Regular Expressions
--   ## Numbers
pBin = some cBinit
pOct = some cOctit
pDec = some cDigit
pHex = some cHexit

pInteger :: RE Char (Literal, Int)
pInteger =  (string "0b" <|> string "0B") *> fmap (from 2)  pBin
        <|> (string "0o" <|> string "0o") *> fmap (from 8)  pOct
        <|> (string "0x" <|> string "0x") *> fmap (from 16) pHex
        <|> fmap (from 10) pDec
  where
    from :: Int -> String -> (Literal, Int)
    from base s = (LInt $ fold base s, length s)

    fold :: Int -> String -> Int
    fold base str = foldl (\s d -> s * base + readDigit d) 0 str

    readDigit :: Char -> Int
    readDigit c = let
      try lo up = let dx = ord c - ord lo
                  in if dx < up then Just dx else Nothing
      x =  try '0' 10
       <|> (+10) <$> try 'A' 6
       <|> (+10) <$> try 'a' 6
      in case x of Just x' -> x'; Nothing -> impossible

pFloat :: RE Char (Literal, Int)
pFloat = let str = pDec <++> string "." <++> pDec
         in fmap ((,) <$> LFloat . read <*> length) str

--   ## Identifiers
pVarId  = cSmall  <:> many cIdChar
pConId  = cLarge  <:> many cIdChar
-- pConSym should always precede pVarSym.
pConSym = sym ':' <:> many cSymbol
pVarSym = some cSymbol

--   ## Special Tokens
pSpecial = cSpecial

--   ## State Transititon Trigger
tString       = string "\""
tChar         = string "'"
tBlockComment = string "{-"

--    # Rules
type TokenResult = Either Trigger (Token, Int)
token :: RE Char TokenResult
token = triggers <|> right vars <|> right lits <|> right pncs
  where
    triggers =  (pure $ Left TrString)  <* tString
            <|> (pure $ Left TrChar)    <* tString
            <|> (pure $ Left TrComment) <* tString
            <|> (pure $ Left TrLine)    <* cLine
            <|> white

    right = fmap Right

    vars =  reserve VarId  keys <$> pVarId
        <|> reserve ConId  []   <$> pConId
        <|> reserve ConSym ops1 <$> pConSym
        <|> reserve ConSym ops2 <$> pVarSym
      where
        reserve con set x = if x `elem` set
                            then (TkRsv x,       length x)
                            else (TkVar (con x), length x)
        keys = [ "let", "in", "where", "case", "of", "do"
               , "if", "then", "else"
               , "infix", "infixl", "infixr"
               , "data", "type", "newtype", "_"
               , "class", "instance", "deriving" ]
        ops1 = [":", "::"]
        ops2 = [ "..", "=", "\\", "|", "<-", "->", "@", "~", "=>" ]

    lits = fmap tokenify (pInteger <|> pFloat)
      where tokenify (lit, w) = (TkLit lit, w)

    pncs = flip fmap pSpecial $ fmap (, 1) TkPnc

    white = (Left . TrWhite . toInt) <$> some cWhite
      where
        toInt = sum . map getInt
        getInt ' '  = 1
        getInt '\t' = 4
        getInt _    = impossible

tokens :: String -> SourcePoint -> Maybe [(Token, ElemPos)]
tokens "" _ = Just []
tokens s  pnt = case findLongestPrefix token s of
  Just (Right (t, w), s') -> let
    pos = unlinkedPos pnt (SourceSpan (0, w))
    in fmap ((t, pos):) $ tokens s' $ endPoint pos
  Just (Left trigger, s') -> case trigger of
    TrWhite w -> tokens s' $ incCol w pnt
    TrLine    -> tokens s' $ SourcePoint (1 + srcRow pnt, 0)
    _         -> error "not supported yet"
  Nothing -> Nothing

lexTokens :: String -> Maybe [(Token, ElemPos)]
lexTokens = flip tokens $ SourcePoint (0, 0)

--    # Postprocessing
--   ## Layout Resolution
resolveLayout :: [(Token, ElemPos)] -> Maybe [(Token, ElemPos)]
resolveLayout = flip layout [] . insertIndents

--  ### Insert Indents
data Indent = NextLv Int
            | SameLv Int
            deriving (Eq, Show)

insertIndents :: [(Token, ElemPos)] -> [Either Indent (Token, ElemPos)]
insertIndents []     = []
insertIndents (tok:toks) = case indent (tok:toks) of
  Just x  -> Right tok : Left x : insertIndents toks
  Nothing -> Right tok : insertIndents toks
  where
    isBlockStart (TkRsv s, _) = s `elem` ["let", "where", "do", "of"]
    isBlockStart _            = False

    indent :: [(Token, ElemPos)] -> Maybe Indent
    indent (t:ts)
      | isBlockStart t, [] <- ts  = Just $ NextLv 0
      | isBlockStart t, ((_, p) : _) <- ts
                                  = Just $ NextLv $ startCol p

    indent ((_, p1) : (_, p2) : _)
      | startRow p1 < startRow p2 = Just $ SameLv $ startCol p2
    indent _ = Nothing

--  ### Layout Algorithm
-- A direct translation of the layout algorithm presented
-- in the Haskell 2010 Language Report.
layout :: [Either Indent (Token, ElemPos)] -> [Int] -> Maybe [(Token, ElemPos)]
-- L (<n>:ts) (m:ms) = ; : (L ts (m:ms))   if m = n
--                   = } : (L (<n>):ts ms) if n < m
-- L (<n>:ts) ms     = L ts ms
layout tts@((Left (SameLv n)):ts) (m:ms)
  | m == n = (semicolon:) <$> layout ts  (m:ms)
  | n < m  = (rbrace:)    <$> layout tts ms
layout ((Left (SameLv _)):ts) ms
                            = layout ts  ms

-- L ({n}:ts) (m:ms) = { : (L ts (n:m:ms)) if n > m
-- L ({n}:ts) []     = { : (L ts [n])      if n > 0
-- L ({n}:ts) ms     = { : } : (L (<n>:ts) ms)
layout ((Left (NextLv n)):ts) (m:ms)
  | n > m  = (lbrace:)    <$> layout ts  (n:m:ms)
layout ((Left (NextLv n)):ts) []
  | n > 0  = layout ts  [n]
layout ((Left (NextLv n)):ts) ms = do
  rest <- layout (Left (SameLv n):ts) ms
  return $ lbrace:rbrace:rest

-- L (}:ts) (0:ms) = } : (L ts ms)
-- L (}:ts) ms     = parse-error
-- L ({:ts) ms     = { : (L ts (0:ms))
-- L (t:ts) (m:ms) = } : (L (t:ts) ms) if m ≠ 0 and parse-error(t)
layout ((Right    (TkRsv "}", _)):ts) (0:ms)
           = (rbrace:)    <$> layout ts  ms
layout ((Right    (TkRsv "}", _)):_)  _
           = Nothing
layout ((Right lb@(TkRsv "{", _)):ts) ms
           = (lb:)        <$> layout ts  (0:ms)
layout tts@(t:_) (m:ms)
  | m /= 0, Nothing <- layout [t] (m:ms)
           = (rbrace:)    <$> layout tts ms

-- L (t:ts) ms = t : (L ts ms)
-- L []     [] = []
-- L [] (m:ms) = } : (L [] ms) if m≠0
layout ((Right t):ts) ms = (t:) <$> layout ts ms
layout [] [] = Just []
layout [] (m:ms)
  | m /= 0 = (rbrace:) <$> layout [] ms
layout _  _  = impossible

semicolon = (TkRsv ";", nullPos pesudoPoint Nothing)
lbrace    = (TkRsv "{", nullPos pesudoPoint Nothing)
rbrace    = (TkRsv "}", nullPos pesudoPoint Nothing)

--   ## Position Resolution
resolvePosition :: [(Token, ElemPos)] -> [(Token, ElemPos)]
resolvePosition = fst . resolve
  where
    resolve :: [(Token, ElemPos)] -> ([(Token, ElemPos)], ElemPos)
    resolve []         = ([], nullPos pesudoPoint Nothing)
    resolve ((t1, p1) : (t2, _) : xs)
      | elem t2 $ map TkRsv [";", "{", "}"] = let
        (xs', ps') = resolve xs
        p2'        = nullPos (if t2 == TkRsv "{"
                              then startPoint ps'
                              else endPoint   p1) (Just ps')
        p1'        = p1 { nextElem = Just p2' }
        in ((t1, p1') : (t2, p2') : xs', p1')
    resolve ((t, p):xs) = let
      (ts', ps') = resolve xs
      p'         = p { nextElem = Just ps' }
      in ((t, p') : ts', p')

--    # Interface
--   ## Interface Datatypes
data Token = TkLit Literal
           | TkVar Variable
           | TkRsv String
           | TkPnc Char
           deriving (Eq)

type PosedToken = (Token, ElemPos)

--   ## Interface Functions
lexIndescript :: String -> Maybe [(Token, ElemPos)]
lexIndescript src = lexTokens src >>= resolveLayout >>= (pure . resolvePosition)

--    # Regex.Applicative combinator extension
symClass :: [Char] -> RE Char Char
symClass cls = psym (\x -> x `elem` cls)

(<++>) = liftA2 (++)
(<:>)  = liftA2 (:)

infixr <++>
infixr <:>

--    # Helper Datatypes and Funcitons

data Trigger = TrString
             | TrChar
             | TrComment
             | TrWhite Int
             | TrLine
             deriving (Eq, Show)

unlinkedPos :: SourcePoint -> SourceSpan -> ElemPos
unlinkedPos p s = ElemPos p s Nothing

-- TODO: Move to a general module.
impossible = error "Confident impossibility."
