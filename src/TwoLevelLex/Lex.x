{
module TwoLevelLex.Lex
       (LexemeClass (..)
       , Lexeme (..)
       , twollLexer
       ) where

}

%wrapper "monad"

$digit = 0-9
$hexdigit = [A-Fa-f0-9]
$alpha    = [A-Za-z]
$alphanum = [$alpha$digit]

@number = [$digit]+|0x[$hexdigit]+
@identifier = ($alpha|_)($alphanum|_|\'|\?)*

twoll :-

$white+ ;
"//".* ;
\->                 { mkL LexArrow }
\(                  { mkL LexLParen }
\)                  { mkL LexRParen }
\.                  { mkL LexDot }
\,                  { mkL LexComma }
\[                  { mkL LexLBrack }
\]                  { mkL LexRBrack }
\=                  { mkL LexEqual }
\#                  { mkL LexOctothorpe }
\:                  { mkL LexColon }

asm                 { mkL LexAsm }
int                 { mkL LexInt }
let                 { mkL LexLet }
in                  { mkL LexIn }
\\                  { mkL LexLambda }

add                 { mkL LexAdd }
breq                { mkL LexBreq }
brlt                { mkL LexBrlt }

@number             { mkInteger }
@identifier         { mkIdentifier }

{

data Lexeme = L LexemeClass String

data LexemeClass = LexArrow
                 | LexLet
                 | LexIn
                 | LexLambda
                 | LexLParen
                 | LexRParen
                 | LexLBrack
                 | LexRBrack
                 | LexEqual
                 | LexColon
                 | LexOctothorpe
                 | LexAsm
                 | LexAdd
                 | LexBreq
                 | LexBrlt
                 | LexDot
                 | LexComma
                 | LexInt
                 | LexLiteralInteger Integer
                 | LexId String
                   deriving Show

mkL :: LexemeClass -> AlexInput -> Int -> Alex (Maybe Lexeme)
mkL l (_p,_c,_bs,s) n = return . Just $ L l (take n s)

mkIdentifier :: AlexInput -> Int -> Alex (Maybe Lexeme)
mkIdentifier (_p,_c,_bs,s) n = return . Just $ L (LexId ident) ident
  where ident = take n s

mkInteger :: AlexInput -> Int -> Alex (Maybe Lexeme)
mkInteger (_p,_c,_bs,s) n = return . Just $ L (LexLiteralInteger (read num)) num
  where num = take n s

alexEOF :: Alex (Maybe Lexeme)
alexEOF = return Nothing

tokenize :: Alex [Lexeme]
tokenize = do
  tok <- alexMonadScan
  case tok of
    Nothing -> return []
    Just t -> do
      ts <- tokenize
      return $ t : ts

twollLexer :: String -> Either String [Lexeme]
twollLexer contents = runAlex contents tokenize

}
