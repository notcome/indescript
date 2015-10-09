{
{-# OPTIONS_GHC -fno-warn-tabs #-}
module Language.Indescript.Lexer where

import Data.Char (toLower, readLitChar)
import Prelude hiding (lex)
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
$white   = [$newline $space $tab]

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
<0> $white+            { tokWhite          }
<0> "--" [^$symbol] .* { tokComment        }

<0> @varid             { tokId    TkVarId  }
<0> @conid             { tokId    TkConId  }
<0> @varsym            { tokSym   TkVarSym }
<0> @consym            { tokSym   TkConSym }
<0> @special           { tokPunc           }
<0> @integer           { tokInt            }
<0> @float             { tokFloat          }

<0>      \"            { begin string      }
<string> [^\"]+        { tokString         }
<string> \"            { begin 0           }
-- " -- Fix Atom’s code highlight
<0>      \'            { begin char        }
<char>   [^\']+        { tokChar           }
<char>   \'            { begin 0           }

{
alexEOF :: Alex PosToken
alexEOF = return (TokenPos (-1) (-1) (-1), TkEOF)

tok :: (String -> Token) -> AlexInput -> Int -> Alex PosToken
tok f (p, _, _, s) len = return (alexPosnToTokenPos p, f $ take len s)

tokWhite   = tok $ const TkWhite
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

data Token = TkLiteral   Literal
           | TkVarId     String
           | TkVarSym    String
           | TkConId     String
           | TkConSym    String
           | TkReserved  String
           | TkLParen    ParenType
           | TkRParen    ParenType
           | TkWhite
           | TkComment
           | TkBacktick
           | TkComma
           | TkSemicolon
           | TkEOF
           deriving (Eq, Show)

data TokenPos = TokenPos
              { tokenOffset :: Int
              , tokenLine   :: Int
              , tokenColumn :: Int
              } deriving (Eq, Show)

alexPosnToTokenPos (AlexPn offset line column) = TokenPos offset line column

type PosToken = (TokenPos, Token)

data Literal = LInt    Int
             | LFloat  Float
             | LChar   Char
             | LString String
             deriving (Eq, Show)

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


lex :: String -> Either String [PosToken]
lex input = runAlex input loop
  where loop = do pt <- alexMonadScan
                  case pt of
                    (_, TkEOF) -> return [pt]
                    otherwise  -> fmap (pt:) loop

lexFile :: FilePath -> IO (Either String [PosToken])
lexFile = fmap lex . readFile
}
