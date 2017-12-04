module Token where

data TOKEN =    TOKEN_READSTRING |
                TOKEN_READINT |    
                TOKEN_PRINT |
                TOKEN_ISNIL |
                TOKEN_HD |
                TOKEN_TL |
                TOKEN_CONS |
                TOKEN_NIL |
                TOKEN_DOT |
                TOKEN_WITH |
                TOKEN_LET |
                TOKEN_PLUS |
                TOKEN_MINUS |
                TOKEN_IDENTIFIER String |
                TOKEN_TIMES |
                TOKEN_DIVIDE |
                TOKEN_INT Int |
                TOKEN_LPAREN |
                TOKEN_RPAREN |
                TOKEN_AND |
                TOKEN_OR |
                TOKEN_EQ |
                TOKEN_NEQ |
                TOKEN_GT |
                TOKEN_GEQ |
                TOKEN_LT |
                TOKEN_LEQ |
                TOKEN_IF |
                TOKEN_THEN |
                TOKEN_ELSE |
                TOKEN_LAMBDA |
                TOKEN_FUN |
                TOKEN_COMMA |
                TOKEN_STRING String |
                TOKEN_IN |
                TOKEN_ERROR String
        deriving (Eq) 

instance Show TOKEN where
    show TOKEN_READSTRING             = "TOKEN_READSTRING"
    show TOKEN_READINT                = "TOKEN_READINT"
    show TOKEN_PRINT                  = "TOKEN_PRINT"
    show TOKEN_ISNIL                  = "TOKEN_ISNIL"
    show TOKEN_HD                     = "TOKEN_HD"
    show TOKEN_TL                     = "TOKEN_TL"
    show TOKEN_CONS                   = "TOKEN_CONS"
    show TOKEN_NIL                    = "TOKEN_NIL"
    show TOKEN_DOT                    = "TOKEN_DOT"
    show TOKEN_WITH                   = "TOKEN_WITH"
    show TOKEN_LET                    = "TOKEN_LET"
    show TOKEN_PLUS                   = "TOKEN_PLUS"
    show TOKEN_MINUS                  = "TOKEN_MINUS"
    show (TOKEN_IDENTIFIER id)        = "TOKEN_IDENTIFIER: " ++ id
    show TOKEN_TIMES                  = "TOKEN_TIMES"
    show TOKEN_DIVIDE                 = "TOKEN_DIVIDE"
    show (TOKEN_INT num)              = "TOKEN_INT: " ++ (show num)
    show TOKEN_LPAREN                 = "TOKEN_LPAREN"
    show TOKEN_RPAREN                 = "TOKEN_RPAREN"
    show TOKEN_AND                    = "TOKEN_AND"
    show TOKEN_OR                     = "TOKEN_OR"
    show TOKEN_EQ                     = "TOKEN_EQ"
    show TOKEN_NEQ                    = "TOKEN_NEQ"
    show TOKEN_GT                     = "TOKEN_GT"
    show TOKEN_GEQ                    = "TOKEN_GEQ"
    show TOKEN_LT                     = "TOKEN_LT"
    show TOKEN_LEQ                    = "TOKEN_LEQ"
    show TOKEN_IF                     = "TOKEN_IF"
    show TOKEN_THEN                   = "TOKEN_THEN"
    show TOKEN_ELSE                   = "TOKEN_ELSE"
    show TOKEN_LAMBDA                 = "TOKEN_LAMBDA"
    show TOKEN_FUN                    = "TOKEN_FUN"
    show TOKEN_COMMA                  = "TOKEN_COMMA"
    show (TOKEN_STRING str)           = "TOKEN_STRING: " ++ "\"" ++ str ++ "\""
    show TOKEN_IN                     = "TOKEN_IN"
    show (TOKEN_ERROR err)            = "TOKEN_ERROR: " ++ err
