-- Lexer for the L language
--
-- Lexer is based off the Tiger lexer from the Alex examples
-- https://github.com/simonmar/alex/blob/master/examples/tiger.x

{
module Lexer where

import Control.Monad
import Data.Maybe
import Numeric ( readDec )
import Data.Char
import Data.Map ( Map )
import qualified Data.Map as Map ( empty )
}

%wrapper "monadUserState"

$digit      = 0-9                                            -- digits
$alpha      = [A-Za-z_]

@number     = [$digit]+
@identifier = $alpha($alpha|_|$digit)*

state:-

<0>             "readstring" { mkT TOKEN_READSTRING}
<0>             "readint"    { mkT TOKEN_READINT }
<0>             "print"      { mkT TOKEN_PRINT }
<0>             "isnil"      { mkT TOKEN_ISNIL }
<0>             \!           { mkT TOKEN_HD }
<0>             \#           { mkT TOKEN_TL }
<0>             \@           { mkT TOKEN_CONS }
<0>             "nil"        { mkT TOKEN_NIL }
<0>             "."          { mkT TOKEN_DOT }
<0>             "with"       { mkT TOKEN_WITH }
<0>             "let"        { mkT TOKEN_LET }
<0>             \+           { mkT TOKEN_PLUS }
<0>             \-           { mkT TOKEN_MINUS }
<0>             "lambda"     { mkT TOKEN_LAMBDA }
<0>             \*           { mkT TOKEN_TIMES }
<0>             \/           { mkT TOKEN_DIVIDE }
<0>             \(           { mkT TOKEN_LPAREN }
<0>             \)           { mkT TOKEN_RPAREN }
<0>             \&           { mkT TOKEN_AND }
<0>             \|           { mkT TOKEN_OR }
<0>             \=           { mkT TOKEN_EQ }
<0>             \<\>         { mkT TOKEN_NEQ }
<0>             \>           { mkT TOKEN_GT }
<0>             \>\=         { mkT TOKEN_GEQ }
<0>             \<           { mkT TOKEN_LT }
<0>             \<\=         { mkT TOKEN_LEQ }
<0>             "if"         { mkT TOKEN_IF }
<0>             "then"       { mkT TOKEN_THEN }
<0>             "else"       { mkT TOKEN_ELSE }
<0>             "fun"        { mkT TOKEN_FUN }
<0>             \,           { mkT TOKEN_COMMA }
<0>             \"           { enterNewString `andBegin` state_string }
<state_string>  \\n          { addCharToString '\n' }
<state_string>  \\t          { addCharToString '\t' }
<state_string>  \\$digit$digit$digit
                             { addAsciiToString }
<state_string>  \\\"         { addCharToString '\"' }
<state_string>  \\\\         { addCharToString '\\' }
<state_string>  \"           { leaveString `andBegin` state_initial }
<state_string>  .            { addCurrentToString }
<state_string>  \n           { skip }
<0>             \n           { skip }
<0>             "in"         { mkT TOKEN_IN }
<0>             "(*"         { enterNewComment `andBegin` state_comment }
<state_comment> "(*"         { embedComment }
<state_comment> "*)"         { unembedComment }
<state_comment> .            ;
<state_comment> \n           { skip }
<0>             $white+      ;
<0>             @identifier  { getVariable }
<0>             @number      { getInteger }
<0>             .            { \_ _ -> lexerError "Illegal character" }

{
-- The token type

data Token = Token AlexPosn TokenClass

instance Show Token where
  show (Token _ TOKEN_EOF) = " TOKEN EOF"
  show (Token p cl)      = show cl ++ showap p
    where
      showap pp = " posn=" ++ showPosn pp

tokPosn :: Token -> AlexPosn
tokPosn (Token p _) = p

tokClass :: Token -> TokenClass
tokClass (Token _ cl) = cl

data TokenClass =
      TOKEN_READSTRING
      | TOKEN_READINT
      | TOKEN_PRINT
      | TOKEN_ISNIL
      | TOKEN_HD
      | TOKEN_TL
      | TOKEN_CONS
      | TOKEN_NIL
      | TOKEN_DOT
      | TOKEN_WITH
      | TOKEN_LET
      | TOKEN_PLUS
      | TOKEN_MINUS
      | TOKEN_IDENTIFIER String
      | TOKEN_TIMES
      | TOKEN_DIVIDE
      | TOKEN_INT        Int
      | TOKEN_LPAREN
      | TOKEN_RPAREN
      | TOKEN_AND
      | TOKEN_OR
      | TOKEN_EQ
      | TOKEN_NEQ
      | TOKEN_GT
      | TOKEN_GEQ
      | TOKEN_LT
      | TOKEN_LEQ
      | TOKEN_IF
      | TOKEN_THEN
      | TOKEN_ELSE
      | TOKEN_LAMBDA
      | TOKEN_FUN
      | TOKEN_COMMA
      | TOKEN_STRING     String
      | TOKEN_IN
      | TOKEN_ERROR      String
      | TOKEN_EOF
      deriving (Show, Eq)

mkT :: TokenClass -> AlexInput -> Int -> Alex Token
mkT c (p, _, _, str) len = return (Token p c)

-- states

state_initial :: Int
state_initial = 0

-- actions

enterNewComment, embedComment, unembedComment :: Action
enterNewString, leaveString, addCurrentToString, addAsciiToString, addControlToString :: Action
getInteger, getVariable :: Action

enterNewComment input len =
    do setLexerCommentDepth 1
       skip input len

embedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd + 1)
       skip input len

unembedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd - 1)
       when (cd == 1) (alexSetStartCode state_initial)
       skip input len

enterNewString _     _   =
    do setLexerStringState True
       setLexerStringValue ""
       alexMonadScan

addCharToString :: Char -> Action
addCharToString c _     _   =
    do addCharToLexerStringValue c
       alexMonadScan

addCurrentToString i@(_, _, _, input) len = addCharToString c i len
  where
    c = if (len == 1)
           then head input
           else error "Invalid call to addCurrentToString''"

-- if we are given the special form '\nnn'
addAsciiToString i@(_, _, _, input) len = if (v < 256)
                                          then addCharToString c i len
                                          else lexerError ("Invalid ascii value : " ++ input)
  where
    s = if (len == 4)
           then drop 1 input
           else error "Invalid call to 'addAsciiToString'"
    r = readDec s
    v = if (length r == 1)
           then fst (head r)
           else error "Invalid call to 'addAsciiToString'"
    c = chr v

-- if we are given the special form '\^A'
addControlToString i@(_, _, _, input) len = addCharToString c' i len
  where
    c = if (len == 1)
           then head input
           else error "Invalid call to 'addControlToString'"
    v = ord c
    c' = if (v >= 64)
            then chr (v - 64)
            else error "Invalid call to 'addControlToString'"

leaveString (p, _, _, input) len =
    do s <- getLexerStringValue
       setLexerStringState False
       return (Token p (TOKEN_STRING (reverse s)))


getInteger (p, _, _, input) len = if (length r == 1)
                                  then return (Token p (TOKEN_INT (fst (head r))))
                                  else lexerError "Invalid number"
  where
    s = take len input
    r = readDec s

-- a sequence of letters is an identifier, except for reserved words, which are tested for beforehand
getVariable (p, _, _, input) len = return (Token p (TOKEN_IDENTIFIER s))
  where
    s = take len input


-- The user state monad

data AlexUserState = AlexUserState
                   {
                     -- used by the lexer phase
                       lexerCommentDepth  :: Int
                     , lexerStringState   :: Bool
                     , lexerStringValue   :: String
                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
                   {
                       lexerCommentDepth  = 0
                     , lexerStringState   = False
                     , lexerStringValue   = ""
                   }

getLexerCommentDepth :: Alex Int
getLexerCommentDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCommentDepth ust)

setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerCommentDepth=ss}}, ())

getLexerStringState :: Alex Bool
getLexerStringState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringState ust)

setLexerStringState :: Bool -> Alex ()
setLexerStringState ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringState=ss}}, ())

getLexerStringValue :: Alex String
getLexerStringValue = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringValue ust)

setLexerStringValue :: String -> Alex ()
setLexerStringValue ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=ss}}, ())

addCharToLexerStringValue :: Char -> Alex ()
addCharToLexerStringValue c = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=c:lexerStringValue (alex_ust s)}}, ())

-- utilities

showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ':': show col

type Pos     = Maybe AlexPosn

line_number :: Pos -> (Int, Int)
line_number Nothing                   = (0, 0)
line_number (Just (AlexPn _ lig col)) = (lig, col)

-- definition needed by Alex

{-
alexEOF :: Alex Token
alexEOF = return (Token undefined TOKEN_EOF Nothing)
-}

alexEOF :: Alex Token
alexEOF = return (Token undefined TOKEN_EOF)

{-
-- Execution
scanner :: String -> Either String [Token]
scanner str = let loop = do (t, m) <- alexComplementError alexMonadScan
                            when (isJust m) (lexerError (fromJust m))
                            let tok@(Token _ cl _) = t
                            if (cl == TOKEN_EOF)
                               then do f1 <- getLexerStringState
                                       d2 <- getLexerCommentDepth
                                       if ((not f1) && (d2 == 0))
                                          then return [tok]
                                          else if (f1)
                                               then alexError "String not closed at end of file"
                                               else alexError "Comment not closed at end of file"
                               else do toks <- loop
                                       return (tok : toks)
              in  runAlex str loop


-- we capture the error message in order to complement it with the file position
alexComplementError :: Alex a -> Alex (a, Maybe String)
alexComplementError (Alex al) = Alex (\s -> case al s of
                                                 Right (s', x) -> Right (s', (x, Nothing))
                                                 Left  message -> Right (s, (undefined, Just message)))

-}

scanner :: String -> Either String [Token]
scanner str = let loop = do t <- alexMonadScan
                            let tok@(Token _ cl) = t
                            if (cl == TOKEN_EOF)
                               then do f1 <- getLexerStringState
                                       d2 <- getLexerCommentDepth
                                       if ((not f1) && (d2 == 0))
                                          then return [tok]
                                          else if (f1)
                                               then alexError "String not closed at end of file"
                                               else alexError "Comment not closed at end of file"
                               else do toks <- loop
                                       return (tok : toks)
              in  runAlex str loop


lexer :: (Token -> Alex a) -> Alex a
lexer f = alexMonadScan >>= f

lexerError :: String -> Alex a
lexerError msg =
    do (p, c, _, inp) <- alexGetInput
       let inp1 = filter (/= '\r') (takeWhile (/='\n') inp)
       let inp2 = if (length inp1 > 30)
                     then trim (take 30 inp1)
                     else trim inp1
       let disp = if (null inp)
                     then " at end of file"
                     else if (null inp2)
                             then " before end of line"
                             else " on char " ++ show c ++ " before : '" ++ inp2 ++ "'"
       let disp3 = if (null msg)
                      then "Lexer error"
                      else trim msg
       alexError (disp3 ++ " at " ++ (showPosn p) ++ disp)
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')


--type Action = AlexInput -> Int -> Alex Lexeme
type Action = AlexInput -> Int -> Alex Token


alexShowError :: (Show t, Show t1) => (t, t1, Maybe String) -> Alex a
alexShowError (line, column, e) = lexerError "parse error"

alexGetPosition :: Alex (AlexPosn)
alexGetPosition = Alex $ \s@AlexState{alex_pos=pos} -> Right (s, pos)

progLower :: Bool -> String -> String
progLower _ []          =   []
progLower True (x:xs)
    |   x == '"'    =   x:(progLower False xs)
    |   otherwise   =   (toLower x):(progLower True xs)
progLower False (x:xs)
    |   x == '"'    =   x:(progLower True xs)
    |   otherwise   =   x:(progLower False xs)

}
