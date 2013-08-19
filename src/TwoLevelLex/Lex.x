{
module TwoLevelLex.Lex
       (LexemeClass (..)
       , Lexeme (..)
       , twollLexer
       ) where

}

%wrapper "monadUserState"

$digit = 0-9
$hexdigit = [A-Fa-f0-9]
$alpha    = [A-Za-z]
$alphanum = [$alpha$digit]

@number = [$digit]+|0x[$hexdigit]+
@identifier = ($alpha|_)($alphanum|_|\'|\?)*

twoll :-

$white+ ;
"//".* ;

-- We build a little state machine to recognize "asm (bindings) (asm
-- code)".  We want to switch over to assembly keywords between the
-- second set of parentheses.  The trick is that we have to pass
-- through an arbitrary number of perfectly ordinary tokens before we
-- get to the asm code.
--
-- So the basic idea is: after the "asm" keyword we switch to paren
--  counting startcode. In paren counting startcode, when the count
--  hits zero after the last outermost close paren, we switch to
--  "preasm" startcode.  In the pre-asm startcode as soon as we see an
--  open paren, we switch to assembler startcode.  Finally in
--  assembler when we see the last (outermost) close paren, we switch
--  back to start code 0.
<0> \(                  { mkL LexLParen }
<0> \)                  { mkL LexRParen }
<pcount, assembler> \(  { incPCount `thenAct` mkL LexLParen }
<pcount> \)             { decPCountAndBegin preAsm `thenAct` mkL LexRParen }
<preAsm> \(             { incPCount `thenAct` (mkL LexLParen `andBegin` assembler) }
<assembler> \)          { decPCountAndBegin 0 `thenAct` mkL LexRParen }

<0,pcount> \->          { mkL LexArrow }
<0,pcount> \,           { mkL LexComma }
<0,pcount> \[           { mkL LexLBrack }
<0,pcount> \]           { mkL LexRBrack }
<0,pcount> \:           { mkL LexColon }
<0,pcount> int          { mkL LexInt }

<0> \.                  { mkL LexDot }
<0> \=                  { mkL LexEqual }
<0> \#                  { mkL LexOctothorpe }

<0> asm                 { mkL LexAsm `andBegin` pcount }
<0> let                 { mkL LexLet }
<0> in                  { mkL LexIn }
<0> \\                  { mkL LexLambda }

<assembler> add         { mkL LexAdd }
<assembler> breq        { mkL LexBreq }
<assembler> brlt        { mkL LexBrlt }

@number                 { mkInteger }
@identifier             { mkIdentifier }

{

data AlexUserState = AlexUserState { ustPCount :: Integer }
                     deriving Show

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 0

alexGetUserState :: Alex AlexUserState
alexGetUserState = Alex $ \s -> Right (s, alex_ust s)

alexSetUserState :: AlexUserState -> Alex ()
alexSetUserState us = Alex $ \s -> Right (s { alex_ust = us }, ())

thenAct :: AlexAction a -> AlexAction b -> AlexAction b
(act1 `thenAct` act2) inp len =
  act1 inp len >> act2 inp len

incPCount :: AlexAction ()
incPCount = modPCount (+1)

getPCount :: AlexAction Integer
getPCount _ _ = Alex $ \s -> Right (s, ustPCount $ alex_ust s)

modPCount :: (Integer -> Integer) -> AlexAction ()
modPCount f _ _ = do
  us <- alexGetUserState
  alexSetUserState (AlexUserState . f $ ustPCount us)

decPCount :: AlexAction ()
decPCount = modPCount (\x -> x - 1)

decPCountAndBegin :: Int -> AlexAction ()
decPCountAndBegin newStartCode inp len = do
  decPCount inp len
  c <- getPCount inp len
  if c == 0
    then alexSetStartCode newStartCode
    else return ()

data Lexeme = L LexemeClass String
              deriving Show

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
