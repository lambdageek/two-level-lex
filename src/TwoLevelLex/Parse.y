{
module TwoLevelLex.Parse (parseString) where

import Data.Reversed

import TwoLevelLex.AST

import TwoLevelLex.Lex

}

%name parse Program
%tokentype { Lexeme }
%error { parseError }

%token
"->"       { L LexArrow _ }
"."        { L LexDot _ }
","        { L LexComma _ }
":"        { L LexColon _ }
"["        { L LexLBrack _ }
"]"        { L LexRBrack _ }
"("        { L LexLParen _ }
")"        { L LexRParen _ }
"\\"       { L LexLambda _ }
"let"      { L LexLet _ }
"in"       { L LexIn _ }
"="        { L LexEqual _ }
"#"        { L LexOctothorpe _ }
"int"      { L LexInt _ }
"asm"      { L LexAsm _ }
"add"      { L LexAdd _ }
"breq"     { L LexBreq _ }
"brlt"     { L LexBrlt _ }
ident      { L (LexId $$) _ }
number     { L (LexLiteralInteger $$) _ }

%%

Name :: { Name }
: ident                           { $1 }

Type :: { Ty }
: "int"                           { IntT }
| "(" Types ")" "->" Type         { FunT (reverse $ getReversed $2) $5 }
| "[" Types "]"                   { ProdT . reverse $ getReversed $2 }

sepList(atm,sep)
: {- empty -}                     { Reversed [] }
| atm                             { Reversed [$1] }
| sepList(atm,sep) sep atm        { $1 `consR` $3 }

Types :: { Reversed [Ty] }
: sepList(Type, ",")              { $1 }

Bind :: { ValBind }
: Name ":" Type                   { ($1, $3) }

Bindings :: { Reversed [ValBind] }
: sepList(Bind, ",")              { $1 }

Literal :: { Literal }
: number                          { $1 }

Exprs :: { Reversed [Expr] }
: sepList(Expr, ",")              { $1 }

Expr :: { Expr }
: Literal                         { LitE $1 }
| ident                           { VarE $1 }
| "(" Expr ")"                    { $2 }
| "let" Bind "=" Expr "in" Expr   { LetE $2 $4 $6 }
| "#" number Expr                 { ProjE (nat $2) $3 }
| "\\" "(" Bindings ")" "." Expr  { LamE (reverse $ getReversed $3) $6 }
| "[" Exprs "]"                   { TupleE (reverse $ getReversed $2) }
| "asm" "(" Bindings ")" "(" Instructions ")"
                                  { AsmE (reverse $ getReversed $3)
                                         (reverse $ getReversed $6) }

Instructions :: { Reversed [Asm] }
: Instr                           { Reversed [$1] }
| Instructions Instr              { $1 `consR` $2 }

Instr :: { Asm }
: "add" ident ident ident         { AddI $2 $3 $4 }
| "breq" ident FnApp FnApp        { BrI EqBC $2 $3 $4 }
| "brlt" ident FnApp FnApp        { BrI LtBC $2 $3 $4 }

FnApp :: { (Name, [Name]) }
: ident "(" sepList(ident, ",") ")"
                                  { ($1, reverse $ getReversed $3) }

Program :: { Program }
: Expr { Program $1 }

{
parseError :: [Lexeme] -> a
parseError [] = error "Parse error: no tokens left to parse."
parseError (L l s : _) = error $ "Parse error: unexpected token '" ++ s ++ "' (Lexeme class: " ++ show l ++ ")."

parseString :: String -> Either String Program
parseString contents = case twollLexer contents of
  Left err -> Left $ "Lexer error: " ++ err
  Right ls -> Right $ parse ls

}
