module Token where

data TOKEN =    READSTRING |
                READINT |    
                PRINT |
                ISNIL |
                HD |
                TL |
                CONS |
                NIL |
                DOT |
                WITH |
                LET |
                PLUS |
                MINUS |
                IDENTIFIER String |
                TIMES |
                DIVIDE |
                INT Int |
                LPAREN |
                RPAREN |
                AND |
                OR |
                EQEQ |
                NEQ |
                GTGT |
                GEQ |
                LTLT |
                LEQ |
                IF |
                THEN |
                ELSE |
                LAMBDA |
                FUN |
                COMMA |
                STRING String |
                IN |
                ERROR String
        deriving (Eq) 

instance Show TOKEN where
    show READSTRING             = "READSTRING"
    show READINT                = "READINT"
    show PRINT                  = "PRINT"
    show ISNIL                  = "ISNIL"
    show HD                     = "!"
    show TL                     = "#"
    show CONS                   = "@"
    show NIL                    = "NIL"
    show DOT                    = "."
    show WITH                   = "WITH"
    show LET                    = "LET"
    show PLUS                   = "+"
    show MINUS                  = "-"
    show (IDENTIFIER id)        = id
    show TIMES                  = "*"
    show DIVIDE                 = "/"
    show (INT num)              = show num
    show LPAREN                 = "("
    show RPAREN                 = ")"
    show AND                    = "&"
    show OR                     = "|"
    show EQEQ                   = "="
    show NEQ                    = "<>"
    show GTGT                   = ">"
    show GEQ                    = ">="
    show LTLT                   = "<"
    show LEQ                    = "<="
    show IF                     = "IF"
    show THEN                   = "THEN"
    show ELSE                   = "ELSE"
    show LAMBDA                 = "LAMBDA"
    show FUN                    = "FUN"
    show COMMA                  = ","
    show (STRING str)           = "\"" ++ str ++ "\""
    show IN                     = "IN"
    show (ERROR err)            = "ERROR: " ++ err
