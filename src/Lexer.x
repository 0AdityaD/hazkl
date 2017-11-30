{
module Lexer where
import Token
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  $white+               ;
  $digit+               { \s -> INT (read s) }
  \+                    { \s -> PLUS }

{

scanTokens = alexScanTokens

}

