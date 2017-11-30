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
    show READSTRING             = "readString"
    show READINT                = "readInt"
    show PRINT                  = "print"
    show ISNIL                  = "isNil"
    show HD                     = "!"
    show TL                     = "#"
    show CONS                   = "@"
    show NIL                    = "nil"
    show DOT                    = "."
    show WITH                   = "with"
    show LET                    = "let"
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
    show IF                     = "if"
    show THEN                   = "then"
    show ELSE                   = "else"
    show LAMBDA                 = "lambda"
    show FUN                    = "fun"
    show COMMA                  = ","
    show (STRING str)           = "\"" ++ str ++ "\""
    show IN                     = "in"
    show (ERROR err)            = "ERROR: " ++ err
