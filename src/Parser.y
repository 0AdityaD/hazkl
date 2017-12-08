{
module Parser where
import Lexer
import Grammar
}

%name parser
%tokentype  { Token }

%monad { Alex }
%lexer { lexer } { Token _ TOKEN_EOF}

%token  TOKEN_PRINT         { Token _ TOKEN_PRINT }
%token  TOKEN_EQ            { Token _ TOKEN_EQ }
%token  TOKEN_NEQ           { Token _ TOKEN_NEQ }
%token  TOKEN_LT            { Token _ TOKEN_LT }
%token  TOKEN_LEQ           { Token _ TOKEN_LEQ }
%token  TOKEN_GT            { Token _ TOKEN_GT }
%token  TOKEN_GEQ           { Token _ TOKEN_GEQ }
%token  TOKEN_AND           { Token _ TOKEN_AND }
%token  TOKEN_OR            { Token _ TOKEN_OR }
%token  TOKEN_PLUS          { Token _ TOKEN_PLUS }
%token  TOKEN_MINUS         { Token _ TOKEN_MINUS }
%token  TOKEN_TIMES         { Token _ TOKEN_TIMES }
%token  TOKEN_DIVIDE        { Token _ TOKEN_DIVIDE }
%token  TOKEN_ISNIL         { Token _ TOKEN_ISNIL }
%token  TOKEN_CONS          { Token _ TOKEN_CONS }
%token  TOKEN_HD            { Token _ TOKEN_HD }
%token  TOKEN_TL            { Token _ TOKEN_TL }
%token  TOKEN_LPAREN        { Token _ TOKEN_LPAREN }
%token  TOKEN_RPAREN        { Token _ TOKEN_RPAREN }
%token  TOKEN_READINT       { Token _ TOKEN_READINT }
%token  TOKEN_READSTRING    { Token _ TOKEN_READSTRING }
%token  TOKEN_NIL           { Token _ TOKEN_NIL }
%token  TOKEN_IF            { Token _ TOKEN_IF }
%token  TOKEN_THEN          { Token _ TOKEN_THEN }
%token  TOKEN_ELSE          { Token _ TOKEN_ELSE }
%token  TOKEN_LET           { Token _ TOKEN_LET }
%token  TOKEN_IN            { Token _ TOKEN_IN }
%token  TOKEN_LAMBDA        { Token _ TOKEN_LAMBDA }
%token  TOKEN_DOT           { Token _ TOKEN_DOT }
%token  TOKEN_COMMA         { Token _ TOKEN_COMMA }
%token  TOKEN_FUN           { Token _ TOKEN_FUN }
%token  TOKEN_WITH          { Token _ TOKEN_WITH }
%token  TOKEN_STRING        { Token _ (TOKEN_STRING $$) }
%token  TOKEN_INT           { Token _ (TOKEN_INT $$) }
%token  TOKEN_IDENTIFIER    { Token _ (TOKEN_IDENTIFIER $$) }

%nonassoc Exp
%left TOKEN_PRINT
%left TOKEN_EQ TOKEN_NEQ TOKEN_LT TOKEN_LEQ TOKEN_GT TOKEN_GEQ
%left TOKEN_AND TOKEN_OR
%left TOKEN_PLUS TOKEN_MINUS
%left TOKEN_TIMES TOKEN_DIVIDE
%right TOKEN_ISNIL
%right TOKEN_CONS
%right TOKEN_HD TOKEN_TL

%%

Exp     :   TOKEN_PRINT Exp                                         { Print $2 }
        |   Exp TOKEN_EQ Exp                                        { EqEq $1 $3 }
        |   Exp TOKEN_NEQ Exp                                       { Neq $1 $3 }
        |   Exp TOKEN_LT Exp                                        { LtLt $1 $3 }
        |   Exp TOKEN_LEQ Exp                                       { Leq $1 $3 }
        |   Exp TOKEN_GT Exp                                        { GtGt $1 $3 }
        |   Exp TOKEN_GEQ Exp                                       { Geq $1 $3 }
        |   Exp TOKEN_AND Exp                                       { And $1 $3 }
        |   Exp TOKEN_OR Exp                                        { Or $1 $3 }
        |   Exp TOKEN_PLUS Exp                                      { Plus $1 $3 }
        |   Exp TOKEN_MINUS Exp                                     { Minus $1 $3 }
        |   Exp TOKEN_TIMES Exp                                     { Times $1 $3 }
        |   Exp TOKEN_DIVIDE Exp                                    { Divide $1 $3 }
        |   TOKEN_ISNIL Exp                                         { Isnil $2 }
        |   Exp TOKEN_CONS Exp                                      { Cons $1 $3 }
        |   TOKEN_HD Exp                                            { HD $2 }
        |   TOKEN_TL Exp                                            { TL $2 }
        |   TOKEN_LPAREN Exp TOKEN_RPAREN                           { $2 }
        |   TOKEN_READINT                                           { ReadInt }
        |   TOKEN_READSTRING                                        { ReadString }
        |   TOKEN_NIL                                               { Nil }
        |   TOKEN_IF Exp TOKEN_THEN Exp TOKEN_ELSE Exp %prec Exp    { Branch $2 $4 $6 }
        |   TOKEN_LET IdConst TOKEN_EQ Exp TOKEN_IN Exp %prec Exp   { Let $2 $4 $6 }
        |   TOKEN_LAMBDA IdList TOKEN_DOT Exp %prec Exp             { FakeLambda $2 $4 }
        |   TOKEN_FUN IdConst TOKEN_WITH IdList TOKEN_EQ Exp TOKEN_IN Exp %prec Exp
                                                                    { Let $2 (FakeLambda $4 $6) $8}
        |   TOKEN_LPAREN App TOKEN_RPAREN                           { Application $2 }
        |   StringConst                                             { ExpString $1 }
        |   IntConst                                                { ExpInt $1 }
        |   IdConst                                                 { ExpId $1 }

IntConst:   TOKEN_INT                                               { Int $1 }

StringConst:    TOKEN_STRING                                        { String $1 }

IdConst:    TOKEN_IDENTIFIER                                        { Id $1 }

IdList  :   IdConst                                                 { [$1] }
        |   IdConst TOKEN_COMMA IdList                              { $1 : $3 }

App     :   Exp Exp                                                 { [$1,$2] }
        |   Exp App                                                 { $1 : $2 }

{

happyError :: Alex a
happyError = do
  (AlexPn _ line col) <- alexGetPosition
  alexShowError (line, col, Nothing)

parse :: String -> Either String Exp
parse s =
    case runAlex s $ parser of
        Left str  -> Left str
        Right exp -> Right exp

}
