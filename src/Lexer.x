{
module Lexer where
import Token
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z_]       -- alphabetic characters

tokens :-

  $white+                       ;
  \"[^\"]*\"                    { \s -> TOKEN_STRING (realString s)}
  "readstring"                  { \s -> TOKEN_READSTRING }
  "readint"                     { \s -> TOKEN_READINT }
  "print"                       { \s -> TOKEN_PRINT }
  "isnil"                       { \s -> TOKEN_ISNIL }
  \!                            { \s -> TOKEN_HD }
  \#                            { \s -> TOKEN_TL }
  \@                            { \s -> TOKEN_CONS }
  "nil"                         { \s -> TOKEN_NIL }
  \.                            { \s -> TOKEN_DOT }
  "with"                        { \s -> TOKEN_WITH }
  "let"                         { \s -> TOKEN_LET }
  \+                            { \s -> TOKEN_PLUS }
  \-                            { \s -> TOKEN_MINUS }
  \*                            { \s -> TOKEN_TIMES }
  \/                            { \s -> TOKEN_DIVIDE }
  \(                            { \s -> TOKEN_LPAREN }
  \)                            { \s -> TOKEN_RPAREN }
  \&                            { \s -> TOKEN_AND }
  \|                            { \s -> TOKEN_OR }
  \=                            { \s -> TOKEN_EQ }
  \<\>                          { \s -> TOKEN_NEQ }
  \>                            { \s -> TOKEN_GT }
  \>\=                          { \s -> TOKEN_GEQ }
  \<                            { \s -> TOKEN_LT }
  \<\=                          { \s -> TOKEN_LEQ }
  "if"                          { \s -> TOKEN_IF }
  "then"                        { \s -> TOKEN_THEN }
  "else"                        { \s -> TOKEN_ELSE }
  "lambda"                      { \s -> TOKEN_LAMBDA }
  "fun"                         { \s -> TOKEN_FUN }
  \,                            { \s -> TOKEN_COMMA }
  "in"                          { \s -> TOKEN_IN }
  $alpha [$alpha $digit]*       { \s -> TOKEN_IDENTIFIER s }
  $digit+                       { \s -> TOKEN_INT (read s) }

{

scanTokens :: String -> [TOKEN]
scanTokens = alexScanTokens

realString :: String -> String
realString s = tail . take (length s - 1) $ s

}

