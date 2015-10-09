tok f p s  = (alexPosnToTokenPos p, (f s))
tokWhite   = tok TkWhite
tokComment = tok TkComment

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
tokPunc = tok tokPunc

tokInt' :: String -> Literal
tokInt' = LInt . parseNum . map toLower
  where
    parseNum :: String -> Int
    parseNum '0':'b':bin = foldl (\s d -> s * 2 + (binToDec d)) 0 bin
    parseNum num         = read num

    binToDec '0' = 0
    binToDec '1' = 1
tokInt = tok (TkLiteral . tokInt')

tokFloat'  = LFloat  . read
tokFloat   = tok (TkLiteral . tokFloat')
tokChar'   = LChar   . read
tokChar    = tok (TkLiteral . tokChar')
tokString' = LString . read
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
           deriving (Eq, Show)

data TokenPos = TokenPos
              { tokenOffset :: Int
              , tokenLine   :: Int
              , tokenColumn :: Int
              }

alexPosnToTokenPos (AlexPn offset line column) = TokenPos offset line column

type PosToken = (TokenPos, Token)

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
                          , "class", "instance", "deriving" ]

reservedOp :: String -> Bool
reservedOp op = op `elem` [ "..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>" ]

floatNumber = 123.456
