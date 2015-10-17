{
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Language.Indescript.Parser.Lexer (
  Token(..), ParenType(..), PosToken
  ) where

import Data.Char (toLower, readLitChar)

import Text.Megaparsec.ShowToken

import Language.Indescript.Syntax
import Language.Indescript.Parser.SourcePos
}

%wrapper "monad"

-- TODO: Understand how GHC stores Unicode characters and add Unicode support

-- Character Sets
$binit = 0-1
$octit = 0-7
$digit = 0-9
$hexit = [$digit A-F a-f]

$small = [a-z \_]
$large = A-Z

$idchar = [$small $large $digit \']

$special = [\(\)\,\;\[\]\`\{\}]
$symbol  = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]

$tab     = [\t]
$space   = [\ ]
$newline = [\n\r\f]
$white   = [$space $tab]

-- Regular Expressions
@bin = $binit+
@oct = $octit+
@dec = $digit+
@hex = $hexit+

-- Negative numbers are handled with partial application on infix operators.
@integer = "0b" @bin | "0B" @bin
         | "0o" @oct | "0O" @oct
         | @dec
         | "0x" @hex | "0X" @hex
@float   = @dec "." @dec

@varid  = $small $idchar*
@conid  = $large $idchar*
@varsym = ($symbol # \:) $symbol*
@consym = \: $symbol*

@special = $special

-- Rules
tokens :-
<0> $white+      { tokWhite          }
<0> $newline     { tokNewline        }
<0> "--" [^$symbol] [^$newline]*
                 { tokComment        }

<0> @varid       { tokId    TkVarId  }
<0> @conid       { tokId    TkConId  }
<0> @varsym      { tokSym   TkVarSym }
<0> @consym      { tokSym   TkConSym }
<0> @special     { tokPunc           }
<0> @integer     { tokInt            }
<0> @float       { tokFloat          }

<0>      \"      { begin string      }
<string> [^\"]+  { tokString         }
<string> \"      { begin 0           }
-- " -- Fix Atom’s code highlight
<0>      \'      { begin char        }
<char>   [^\']+  { tokChar           }
<char>   \'      { begin 0           }

{
data Token = TkLiteral   Literal
           | TkVarId     String
           | TkVarSym    String
           | TkConId     String
           | TkConSym    String
           | TkReserved  String
           | TkLParen    ParenType
           | TkRParen    ParenType
           | TkWhite
           | TkNewline
           | TkComment
           | TkBacktick
           | TkComma
           | TkSemicolon
           deriving (Eq)

type PosToken  = (Token, SourcePos)
type AlexToken = Maybe PosToken

alexEOF :: Alex AlexToken
alexEOF = return Nothing

lexSource :: String -> String -> Either String [PosToken]
lexSource source input = runAlex input loop
  where
    loop = do tok <- alexMonadScan
              case tok of
                Nothing -> return []
                Just x  -> let x' = addSource <$> x
                           in fmap (x:) loop
    addSource pos = pos { sourceName = source }

tok :: (String -> Token) -> AlexInput -> Int -> Alex AlexToken
tok f (pos, _, _, str) len = let
  (AlexPn _ line col) = pos
  tok  = f $ take len str
  pos' = SourcePos "" line col (case tok of
           TkNewline -> (1, 0)
           _         -> (0, len))
  in return $ Just (tok, pos')

tokWhite   = tok $ const TkWhite
tokNewline = tok $ const TkNewline
tokComment = tok $ const TkComment

tokId' :: (String -> Token) -> String -> Token
tokId' con id = if   reservedId id
                then TkReserved id
                else con        id
tokId = tok . tokId'

tokSym' :: (String -> Token) -> String -> Token
tokSym' con sym = if   reservedOp sym
                  then TkReserved sym
                  else con        sym
tokSym = tok . tokSym'

tokPunc' :: String -> Token
tokPunc' "`" = TkBacktick
tokPunc' "," = TkComma
tokPunc' ";" = TkSemicolon
tokPunc' "(" = TkLParen CircleParen
tokPunc' ")" = TkRParen CircleParen
tokPunc' "[" = TkLParen SquareParen
tokPunc' "]" = TkRParen SquareParen
tokPunc' "{" = TkLParen CurlyParen
tokPunc' "}" = TkRParen CurlyParen
tokPunc = tok tokPunc'

tokInt' :: String -> Literal
tokInt' = LInt . parseNum . map toLower
  where
    parseNum :: String -> Int
    parseNum ('0':'b':bin) = foldl (\s d -> s * 2 + (binToDec d)) 0 bin
    parseNum num           = read num

    binToDec '0' = 0
    binToDec '1' = 1
tokInt = tok (TkLiteral . tokInt')

readChar = fst . head . readLitChar
readString []  = ""
readString str = let [(x, xs)] = readLitChar str
                 in x:(readString xs)

tokFloat'  = LFloat  . read
tokFloat   = tok (TkLiteral . tokFloat')
tokChar'   = LChar   . readChar
tokChar    = tok (TkLiteral . tokChar')
tokString' = LString . readString
tokString  = tok (TkLiteral . tokString')

data ParenType = CircleParen
               | SquareParen
               | CurlyParen
               deriving (Eq, Show)

reservedId :: String -> Bool
reservedId id = id `elem` [ "let", "in", "where", "case", "of"
                          , "if", "then", "else"
                          , "infix", "infixl", "infixr"
                          , "data", "type", "newtype", "_"
                          , "class", "instance", "deriving" ]

reservedOp :: String -> Bool
reservedOp op = op `elem` [ "..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>" ]

instance Show Token where
  show (TkLiteral  (LInt    x)) = show x
  show (TkLiteral  (LFloat  x)) = show x
  show (TkLiteral  (LChar   x)) = show x
  show (TkLiteral  (LString x)) = show x
  show (TkVarId    x)           = x
  show (TkVarSym   x)           = x
  show (TkConId    x)           = x
  show (TkConSym   x)           = x
  show (TkReserved x)           = x
  show (TkLParen   CircleParen) = "("
  show (TkRParen   CircleParen) = ")"
  show (TkLParen   SquareParen) = "["
  show (TkRParen   SquareParen) = "]"
  show (TkLParen   CurlyParen)  = "{"
  show (TkRParen   CurlyParen)  = "}"
  show TkWhite                  = " "
  show TkNewline                = "\n"
  show TkComment                = "-- lorem ipsum"
  show TkBacktick               = "`"
  show TkComma                  = ","
  show TkSemicolon              = ";"

instance ShowToken Token where
  showToken = show
}
