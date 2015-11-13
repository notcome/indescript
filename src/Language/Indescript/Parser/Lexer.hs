{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Indescript.Parser.Lexer (
    Token(..)
  , LinkedPos
  , PosedToken
  , lexIndescript
) where

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

pInteger :: RE Char (Lit, Int)
pInteger =  (string "0b" <|> string "0B") *> fmap (from 2)  pBin
        <|> (string "0o" <|> string "0o") *> fmap (from 8)  pOct
        <|> (string "0x" <|> string "0x") *> fmap (from 16) pHex
        <|> fmap (from 10) pDec
  where
    from :: Int -> String -> (Lit, Int)
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

pFloat :: RE Char (Lit, Int)
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
            <|> (pure $ Left TrChar)    <* tChar
            <|> (pure $ Left TrComment) <* tBlockComment
            <|> (pure $ Left TrLine)    <* cLine
            <|> white

    right = fmap Right

    vars =  reserve VarId  keys <$> pVarId
        <|> reserve ConId  []   <$> pConId
        <|> reserve ConSym ops1 <$> pConSym
        <|> reserve VarSym ops2 <$> pVarSym
      where
        reserve con set x = if x `elem` set
                            then (TkRsv x,       length x)
                            else (TkVar (con x), length x)
        keys = [ "let", "in", "where", "case", "of", "do"
               , "if", "then", "else"
               , "infix", "infixl", "infixr"
               , "data", "type", "forall", "newtype", "_"
               , "class", "instance", "deriving"
               , "module", "import"]
        ops1 = [":", "::"]
        ops2 = [ "..", "=", "\\", "|", "<-", "->", "@", "~", "=>" ]

    lits = fmap tokenify (pInteger <|> pFloat)
      where tokenify (lit, w) = (TkLit lit, w)

    pncs = flip fmap pSpecial $ fmap (, 1) (TkRsv . (:[]))

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
    pos = ElemPos pnt $ SourceSpan (0, w)
    in fmap ((t, pos):) $ tokens s' $ endPoint pos
  Just (Left trigger, s') -> case trigger of
    TrWhite w -> tokens s' $ incCol w pnt
    TrLine    -> tokens s' $ SourcePoint (1 + srcRow pnt, 1)
    _         -> error "not supported yet"
  Nothing -> Nothing

lexTokens :: String -> Maybe [(Token, ElemPos)]
lexTokens = flip tokens $ SourcePoint (1, 1)

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
-- in the Haskell 2010 Language Report. Modified to handle “in”.

-- Pattern Synonyms
pattern Same n  = Left (SameLv n)
pattern Next n  = Left (NextLv n)
pattern TokP s <- Right (TkRsv s, _)

layout :: [Either Indent (Token, ElemPos)] -> [Int] -> Maybe [(Token, ElemPos)]
-- L (<n>:ts) (m:ms) = ; : (L ts (m:ms))   if m = n
--                   = } : (L (<n>):ts ms) if n < m
-- L (<n>:ts) ms     = L ts ms
layout tts@((Same n):ts) (m:ms)
  | m == n = (semicolon:) <$> layout ts  (m:ms)
  | n < m  = (rbrace:)    <$> layout tts ms
layout ((Same _):ts) ms    =  layout ts  ms

-- L ({n}:ts) (m:ms) = { : (L ts (n:m:ms)) if n > m
-- L ({n}:ts) []     = { : (L ts [n])      if n > 0
-- L ({n}:ts) ms     = { : } : (L (<n>:ts) ms)
layout ((Next n):ts) (m:ms)
  | n > m  = (lbrace:)    <$> layout ts  (n:m:ms)
layout ((Next n):ts) []
  | n > 0  = (lbrace:)    <$> layout ts  [n]
layout ((Next n):ts) ms = do
  rest <- layout (Same n:ts) ms
  return $ lbrace:rbrace:rest

-- Special rule for “in”:
-- L (t:in:ts) (m:ms) = } : L (in:ts ms) if t ≠ { and m ≠ 0
layout (t1:t2:ts) (m:ms)
  | Right t1' <- t1, notRbrace t1
  , TokP "in" <- t2
  , m /= 0 = (\rest -> t1':rbrace:rest) <$> layout (t2:ts) ms
  where notRbrace (TokP "}") = False
        notRbrace _          = True

-- L (}:ts) (0:ms) = } : (L ts ms)
-- L (}:ts) ms     = parse-error
layout ((TokP "}"):ts)   mms
  | 0:ms <- mms = (rbrace:) <$> layout ts ms
  | otherwise   = Nothing

-- L ({:ts) ms     = { : (L ts (0:ms))
-- L (t:ts) (m:ms) = } : (L (t:ts) ms) if m ≠ 0 and parse-error(t)
-- A parse-error only occurs when an explicit right brace matches a non-explicit
-- left brace, as guarded below.
layout (t:ts) mms@(m:ms)
  | TokP "{" <- t, Right lb <- t
    = (lb:)     <$> layout ts     (0:mms)
  | TokP "}" <- t, m /= 0
    = (rbrace:) <$> layout (t:ts) ms

-- L (t:ts) ms = t : (L ts ms)
-- L []     [] = []
-- L [] (m:ms) = } : (L [] ms) if m≠0
layout ((Right t):ts) ms = (t:) <$> layout ts ms
layout [] [] = Just []
layout [] (m:ms)
  | m /= 0 = (rbrace:) <$> layout [] ms
layout _  _  = impossible

semicolon = (TkRsv ";", nullPos pesudoPoint)
lbrace    = (TkRsv "{", nullPos pesudoPoint)
rbrace    = (TkRsv "}", nullPos pesudoPoint)

--   ## Position Resolution
resolvePosition :: [(Token, ElemPos)] -> [PosedToken]
resolvePosition = fst . resolve
  where
    resolve :: [(Token, ElemPos)] -> ([PosedToken], LinkedPos)
    resolve []         = ([], [])
    resolve ((t1, p1) : (t2, p2) : xs)
      | startPoint p2 == pesudoPoint = let
        (xs', ps') = resolve xs
        p2'        = nullPos (if t2 == TkRsv "{"
                              then startPoint $ head ps'
                              else endPoint p1) : ps'
        p1'        = p1:p2'
        in ((t1, p1') : (t2, p2') : xs', p1')
    resolve ((t, p):xs) = let
      (ts', ps') = resolve xs
      p'         = p:ps'
      in ((t, p') : ts', p')

--    # Interface
--   ## Interface Datatypes
data Token = TkLit Lit
           | TkVar Var
           | TkRsv String
           deriving (Eq, Show)

type LinkedPos  = [ElemPos]
type PosedToken = (Token, LinkedPos)

--   ## Interface Functions
lexIndescript :: String -> Maybe [PosedToken]
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

-- TODO: Move to a general module.
impossible = error "Confident impossibility."
