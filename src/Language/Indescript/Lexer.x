{
  module Language.Indescript.Lexer where
}

%wrapper "posn"

-- TODO: Understand how GHC stores Unicode characters and add Unicode support

-- Character Sets
$binit = 0-1
$octit = 0-7
$decit = 0-9
$hexit = [$decit A-F a-f]

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
@dec = $decit+
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
$consym = \: $symbol*

-- Rules
tokens :-
<0> $white+              ;
<0> "--" [^$symbol] .*   ;
<0> @varid             { tok (\s -> }
<0> @conid
<0> @varsym
<0> @consym




$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-
  $white+       ;


  $white+				;
  "--".*				;
  let					{ tok (\p s -> Let p) }
  in					{ tok (\p s -> In p) }
  $digit+				{ tok (\p s -> Int p (read s)) }
  [\=\+\-\*\/\(\)]			{ tok (\p s -> Sym p (head s)) }
  $alpha [$alpha $digit \_ \']*		{ tok (\p s -> Var p s) }

{
tok f p s = f p s

data Token = TkLiteral   Literal
           | TkVarId     String
           | TkVarSym    String
           | TkConId     String
           | TkConSym    String
           | TkReserved  String
           | TkLParen    ParenType
           | TkRParen    ParenType
           | TkSemicolon
           deriving (Eq, Show)

type PosToken = (AlexPosn, Token)

data Literal = LInt    Int
             | LFloat  Float
             | LChar   Char
             | LString String
             deriving (Eq, Show)

data ParenType = CircleParen
               = SquareParen
               = CurlyParen
               deriving (Eq, Show)

reservedId :: String -> Bool
reservedId id = id `elem` [ "let", "in", "where", "case", "of"
                          , "if", "then", "else"
                          , "infix", "infixl", "infixr"
                          , "data", "type", "newtype", "_"
                          , "class", "instance", "deriving"]

reservedOp :: String -> Bool
reservedOp op = op `elem` [ "..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]
}
