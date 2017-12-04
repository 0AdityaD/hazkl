{
module Parser where
import Token
import Grammar
}

%name parse
%tokentype  { TOKEN }
%error      { parseError }

%token  TOKEN_PRINT         { TOKEN_PRINT }
%token  TOKEN_EQ            { TOKEN_EQ }
%token  TOKEN_NEQ           { TOKEN_NEQ }
%token  TOKEN_LT            { TOKEN_LT }
%token  TOKEN_LEQ           { TOKEN_LEQ }
%token  TOKEN_GT            { TOKEN_GT }
%token  TOKEN_GEQ           { TOKEN_GEQ }
%token  TOKEN_AND           { TOKEN_AND }
%token  TOKEN_OR            { TOKEN_OR }
%token  TOKEN_PLUS          { TOKEN_PLUS }
%token  TOKEN_MINUS         { TOKEN_MINUS }
%token  TOKEN_TIMES         { TOKEN_TIMES }
%token  TOKEN_DIVIDE        { TOKEN_DIVIDE }
%token  TOKEN_ISNIL         { TOKEN_ISNIL }
%token  TOKEN_CONS          { TOKEN_CONS }
%token  TOKEN_HD            { TOKEN_HD }
%token  TOKEN_TL            { TOKEN_TL }
%token  TOKEN_LPAREN        { TOKEN_LPAREN }
%token  TOKEN_RPAREN        { TOKEN_RPAREN }
%token  TOKEN_READINT       { TOKEN_READINT }
%token  TOKEN_READSTRING    { TOKEN_READSTRING }
%token  TOKEN_NIL           { TOKEN_NIL }
%token  TOKEN_IF            { TOKEN_IF }
%token  TOKEN_THEN          { TOKEN_THEN }
%token  TOKEN_ELSE          { TOKEN_ELSE }
%token  TOKEN_LET           { TOKEN_LET }
%token  TOKEN_IN            { TOKEN_IN }
%token  TOKEN_LAMBDA        { TOKEN_LAMBDA }
%token  TOKEN_DOT           { TOKEN_DOT }
%token  TOKEN_COMMA         { TOKEN_COMMA }
%token  TOKEN_FUN           { TOKEN_FUN }
%token  TOKEN_WITH          { TOKEN_WITH }
%token  TOKEN_STRING        { TOKEN_STRING $$ }
%token  TOKEN_INT           { TOKEN_INT $$ }
%token  TOKEN_IDENTIFIER    { TOKEN_IDENTIFIER $$ }

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

parseError :: [TOKEN] -> a
parseError _ = error "Parse error"

}
