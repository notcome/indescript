{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE TupleSections #-}

module Language.Indescript.Parser.Lexer where

import Data.Char              (ord)
import Text.Regex.Applicative

import Language.Indescript.Syntax

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

pInteger :: RE Char (Literal, Width)
pInteger =  (string "0b" <|> string "0B") *> fmap (from 2)  pBin
        <|> (string "0o" <|> string "0o") *> fmap (from 8)  pOct
        <|> (string "0x" <|> string "0x") *> fmap (from 16) pHex
        <|> fmap (from 10) pDec
  where
    from :: Int -> String -> (Literal, Width)
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

pFloat :: RE Char (Literal, Width)
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
type TokenResult = Either Trigger (Token, Width)
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

    white = (Left . TrWhite . toWidth) <$> some cWhite
      where
        toWidth = sum . map getWidth
        getWidth ' '  = 1
        getWidth '\t' = 4
        getWidth _    = impossible

-- TODO: add escape charaters support.
-- TODO: add multiline string support.
cpsPString :: String -> LCPos -> Maybe [(Token, LCWPos)]
cpsPString s (l, c) = case findLongestPrefix pString s of
  Just (str, s') -> let
    w  = length str + 2
    t  = TkLit $ LString str
    t' = (t, (l, c, w))
    ts = tokens s' (l, c + w)
    in fmap (t':) ts
  Nothing -> Nothing
  where
    pString = sym '"' *> many (psym $ \x -> x /= '"') <* sym '"'

tokens :: String -> LCPos -> Maybe [(Token, LCWPos)]
tokens "" _      = Just []
tokens s  (l, c) = case findLongestPrefix token s of
  Just (Right (t, w), s') ->
    fmap ((t, (l, c, w)):) $ tokens s' (l, c + w)
  Just (Left trigger, s') -> case trigger of
    TrWhite w -> tokens s' (l, c + w)
    TrLine    -> tokens s' (l + 1, 0)
    TrString  -> cpsPString s (l, c)
    _         -> error "not supported yet"
  Nothing -> Nothing

lexTokens :: String -> Maybe [(Token, LCWPos)]
lexTokens = flip tokens (0, 0)

--    # Postprocessing
--   ## Layout Resolution
resolveLayout :: [(Token, LCWPos)] -> Maybe [(Token, LCWPos)]
resolveLayout = flip layout [] . insertIndents

--  ### Insert Indents
data Indent = NextLv Int
            | SameLv Int
            deriving (Eq, Show)

insertIndents :: [(Token, LCWPos)] -> [Either Indent (Token, LCWPos)]
insertIndents []     = []
insertIndents (tok:toks) = case indent (tok:toks) of
  Just x  -> Right tok : Left x : insertIndents toks
  Nothing -> Right tok : insertIndents toks
  where
    isBlockStart (TkRsv s, _) = s `elem` ["let", "where", "do", "of"]
    isBlockStart _            = False

    indent (t:ts)
      | isBlockStart t, [] <- ts = Just $ NextLv 0
      | isBlockStart t, ((_, (_, c, _)):_) <- ts
                                 = Just $ NextLv c
    indent ((_, (l1, _, _)):(_, (l2, c, _)):_)
      | l1 < l2                  = Just $ SameLv c
    indent _ = Nothing

--  ### Layout Algorithm
-- A direct translation of the layout algorithm presented
-- in the Haskell 2010 Language Report.
layout :: [Either Indent (Token, LCWPos)] -> [Int] -> Maybe [(Token, LCWPos)]
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

fakePos :: LCWPos
fakePos = (-1, -1, 0)
semicolon = (TkRsv ":", fakePos)
lbrace    = (TkRsv "{", fakePos)
rbrace    = (TkRsv "}", fakePos)

--   ## Position Resolution
resolvePosition :: [(Token, LCWPos)] -> [(Token, SourcePos)]
resolvePosition = fst . resolve
  where
    resolve :: [(Token, LCWPos)] -> ([(Token, SourcePos)], SourcePos)
    resolve []          = ([], fromLCWPos fakePos undefined)
    resolve ((t, p):ts) = let
      (ts', next) = resolve ts
      -- TODO: consider a better way to assign position to implicit tokens.
      this        = if p == fakePos
                    then next { nextTokenPos = next }
                    else fromLCWPos p next
      in ((t, this) : ts', this)

--    # Datatypes and Helper Functions
--   ## Datatypes
data Token = TkLit Literal
           | TkVar Variable
           | TkRsv String
           | TkPnc Char
           deriving (Eq)

type PosedToken = (Token, SourcePos)

data Trigger = TrString
             | TrChar
             | TrComment
             | TrWhite Int
             | TrLine
             deriving (Eq, Show)

type Width  = Int
type LCPos  = (Int, Int)
type LCWPos = (Int, Int, Int)

-- TODO: move to Language.Indescript.Parser.SourcePos
data SourcePos = SourcePos
               { sourceLine   :: Int
               , sourceColumn :: Int
               , contentWidth :: Int
               , nextTokenPos :: SourcePos
               } deriving (Eq, Show)

fromLCWPos (l, c, w) next = SourcePos l c w next

--   ## Help Functions
lexIndescript :: String -> Maybe [(Token, SourcePos)]
lexIndescript src = lexTokens src >>= resolveLayout >>= (pure . resolvePosition)

--    # Regex.Applicative combinator extension
symClass :: [Char] -> RE Char Char
symClass cls = psym (\x -> x `elem` cls)

(<++>) = liftA2 (++)
(<:>)  = liftA2 (:)

infixr <++>
infixr <:>

--    # TODO: Move to a general module.
impossible = error "Confident impossibility."
