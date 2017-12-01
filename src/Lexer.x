{
module Lexer where
import Token
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z_]       -- alphabetic characters

tokens :-

  $white+                       ;
  \"[^\"]*\"                     { \s -> STRING (realString s)}
  "readstring"                  { \s -> READSTRING }
  "readint"                     { \s -> READINT }
  "print"                       { \s -> PRINT }
  "isnil"                       { \s -> ISNIL }
  \!                            { \s -> HD }
  \#                            { \s -> TL }
  \@                            { \s -> CONS }
  "nil"                         { \s -> NIL }
  \.                            { \s -> DOT }
  "with"                        { \s -> WITH }
  "let"                         { \s -> LET }
  \+                            { \s -> PLUS }
  \-                            { \s -> MINUS }
  $alpha [$alpha $digit]*       { \s -> IDENTIFIER s }
  \*                            { \s -> TIMES }
  \/                            { \s -> DIVIDE }
  $digit+                       { \s -> INT (read s) }
  \(                            { \s -> LPAREN }
  \)                            { \s -> RPAREN }
  \&                            { \s -> AND }
  \|                            { \s -> OR }
  \=                            { \s -> EQEQ }
  \<\>                          { \s -> NEQ }
  \>                            { \s -> GTGT }
  \>\=                          { \s -> GEQ }
  \<                            { \s -> LTLT }
  \<\=                          { \s -> LEQ }
  "if"                          { \s -> IF }
  "then"                        { \s -> THEN }
  "else"                        { \s -> ELSE }
  "lambda"                      { \s -> LAMBDA }
  "fun"                         { \s -> FUN }
  \,                            { \s -> COMMA }
  "in"                          { \s -> IN }

{

scanTokens = alexScanTokens

realString s = tail . take (length s - 1) $ s

}

