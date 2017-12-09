module Grammar where

data Exp =      Print   Exp             
            |   EqEq    Exp Exp         
            |   Neq     Exp Exp  
            |   LtLt    Exp Exp         
            |   Leq     Exp Exp         
            |   GtGt    Exp Exp         
            |   Geq     Exp Exp         
            |   And     Exp Exp         
            |   Or      Exp Exp         
            |   Plus    Exp Exp         
            |   Minus   Exp Exp         
            |   Times   Exp Exp         
            |   Divide  Exp Exp
            |   Isnil   Exp
            |   Cons    Exp Exp
            |   HD      Exp             
            |   TL      Exp
            |   ReadInt                 
            |   ReadString              
            |   Nil
            |   Branch  Exp Exp Exp     
            |   Let     IdConst Exp Exp
            |   FakeLambda  IdList Exp
            |   Lambda  IdConst Exp
            |   Application App
            |   ExpString StringConst
            |   ExpInt IntConst
            |   ExpId IdConst
            |   Error String
    deriving (Eq)

instance Show Exp where
    show (Print exp1)            = "(print" ++ (show exp1) ++ ")"
    show (EqEq exp1 exp2)        = "(" ++ (show exp1) ++ " = "  ++ (show exp2) ++ ")"
    show (Neq exp1 exp2)         = "(" ++ (show exp1) ++ " <> " ++ (show exp2) ++ ")"
    show (LtLt exp1 exp2)        = "(" ++ (show exp1) ++ " < "  ++ (show exp2) ++ ")"
    show (Leq exp1 exp2)         = "(" ++ (show exp1) ++ " <= " ++ (show exp2) ++ ")"
    show (GtGt exp1 exp2)        = "(" ++ (show exp1) ++ " > "  ++ (show exp2) ++ ")"
    show (Geq exp1 exp2)         = "(" ++ (show exp1) ++ " >= " ++ (show exp2) ++ ")"
    show (And exp1 exp2)         = "(" ++ (show exp1) ++ " & "  ++ (show exp2) ++ ")"
    show (Or exp1 exp2)          = "(" ++ (show exp1) ++ " | "  ++ (show exp2) ++ ")"
    show (Plus exp1 exp2)        = "(" ++ (show exp1) ++ " + "  ++ (show exp2) ++ ")"
    show (Minus exp1 exp2)       = "(" ++ (show exp1) ++ " - "  ++ (show exp2) ++ ")"
    show (Times exp1 exp2)       = "(" ++ (show exp1) ++ " * "  ++ (show exp2) ++ ")"
    show (Divide exp1 exp2)      = "(" ++ (show exp1) ++ " / " ++ (show exp2) ++ ")"
    show (Isnil exp)             = "(IsNil" ++ (show exp) ++ ")"
    show (Cons exp1 exp2)        = "[" ++ (show exp1) ++ ", " ++ (show exp2) ++ "]"
    show (HD exp)                = "(!" ++ (show exp) ++ ")"
    show (TL exp)                = "(#" ++ (show exp) ++ ")"
    show (ReadInt)               = "readInt"
    show (ReadString)            = "readString"
    show (Nil)                   = "Nil"
    show (Branch exp1 exp2 exp3) = "(if " ++ (show exp1) ++ " then " ++ (show exp2) ++ " else " ++ (show exp3) ++ ")"
    show (Let id exp1 exp2)      = "let " ++ (show id) ++ " = " ++ (show exp1) ++ " in " ++ (show exp2)
    show (FakeLambda list exp)   = "TODO fake lambda"
    show (Lambda id exp)         = "lambda " ++ (show id) ++ ". " ++ (showUnevaluatedCons exp)
    show (Application app)       = "(" ++ showApp app ++ ")"
    show (ExpString const)       = show . show $ const
    show (ExpInt const)          = show const
    show (ExpId const)           = show const
    show (Error str)             = str

data IntConst = Int Int
    deriving (Eq, Ord)

instance Show IntConst where
    show (Int int) = show int

data StringConst = String String
    deriving (Eq, Ord)

instance Show StringConst where
    show (String string) = string

data IdConst = Id String
    deriving (Eq, Ord)

instance Show IdConst where
    show (Id str) = str

type IdList = [IdConst]

type App = [Exp]

showApp :: App -> String
showApp [x]     = show x
showApp (x:xs)  = show x ++ " " ++ showApp xs

showUnevaluatedCons :: Exp -> String
showUnevaluatedCons (Cons exp1 exp2)        = "(" ++ showUnevaluatedCons exp1 ++ " @ " ++ showUnevaluatedCons exp2 ++ ")"
showUnevaluatedCons (Print exp1)            = "(print" ++ (showUnevaluatedCons exp1) ++ ")"
showUnevaluatedCons (EqEq exp1 exp2)        = "(" ++ (showUnevaluatedCons exp1) ++ " = "  ++ (showUnevaluatedCons exp2) ++ ")"
showUnevaluatedCons (Neq exp1 exp2)         = "(" ++ (showUnevaluatedCons exp1) ++ " <> " ++ (showUnevaluatedCons exp2) ++ ")"
showUnevaluatedCons (LtLt exp1 exp2)        = "(" ++ (showUnevaluatedCons exp1) ++ " < "  ++ (showUnevaluatedCons exp2) ++ ")"
showUnevaluatedCons (Leq exp1 exp2)         = "(" ++ (showUnevaluatedCons exp1) ++ " <= " ++ (showUnevaluatedCons exp2) ++ ")"
showUnevaluatedCons (GtGt exp1 exp2)        = "(" ++ (showUnevaluatedCons exp1) ++ " > "  ++ (showUnevaluatedCons exp2) ++ ")"
showUnevaluatedCons (Geq exp1 exp2)         = "(" ++ (showUnevaluatedCons exp1) ++ " >= " ++ (showUnevaluatedCons exp2) ++ ")"
showUnevaluatedCons (And exp1 exp2)         = "(" ++ (showUnevaluatedCons exp1) ++ " & "  ++ (showUnevaluatedCons exp2) ++ ")"
showUnevaluatedCons (Or exp1 exp2)          = "(" ++ (showUnevaluatedCons exp1) ++ " | "  ++ (showUnevaluatedCons exp2) ++ ")"
showUnevaluatedCons (Plus exp1 exp2)        = "(" ++ (showUnevaluatedCons exp1) ++ " + "  ++ (showUnevaluatedCons exp2) ++ ")"
showUnevaluatedCons (Minus exp1 exp2)       = "(" ++ (showUnevaluatedCons exp1) ++ " - "  ++ (showUnevaluatedCons exp2) ++ ")"
showUnevaluatedCons (Times exp1 exp2)       = "(" ++ (showUnevaluatedCons exp1) ++ " * "  ++ (showUnevaluatedCons exp2) ++ ")"
showUnevaluatedCons (Divide exp1 exp2)      = "(" ++ (showUnevaluatedCons exp1) ++ " / " ++ (showUnevaluatedCons exp2) ++ ")"
showUnevaluatedCons (Isnil exp)             = "(IsNil" ++ (showUnevaluatedCons exp) ++ ")"
showUnevaluatedCons (HD exp)                = "(!" ++ (showUnevaluatedCons exp) ++ ")"
showUnevaluatedCons (TL exp)                = "(#" ++ (showUnevaluatedCons exp) ++ ")"
showUnevaluatedCons (ReadInt)               = "readInt"
showUnevaluatedCons (ReadString)            = "readString"
showUnevaluatedCons (Nil)                   = "Nil"
showUnevaluatedCons (Branch exp1 exp2 exp3) = "(if " ++ (showUnevaluatedCons exp1) ++ " then " ++ (showUnevaluatedCons exp2) ++
                                                    " else " ++ (showUnevaluatedCons exp3) ++ ")"
showUnevaluatedCons (Let id exp1 exp2)      = "let " ++ (show id) ++ " = " ++ (showUnevaluatedCons exp1) ++ " in " ++ (showUnevaluatedCons exp2)
showUnevaluatedCons (FakeLambda list exp)   = "TODO fake lambda"
showUnevaluatedCons (Lambda id exp)         = "lambda " ++ (show id) ++ ". " ++ (showUnevaluatedCons exp)
showUnevaluatedCons (Application app)       = "(" ++ show app ++ ")"
showUnevaluatedCons (ExpString const)       = show . show $ const
showUnevaluatedCons (ExpInt const)          = show const
showUnevaluatedCons (ExpId const)           = show const

fixLambdas :: Exp -> Exp
fixLambdas (FakeLambda [x] exp)     =   (Lambda x (fixLambdas exp))
fixLambdas (FakeLambda (x:xs) exp)  =   (Lambda x (fixLambdas (FakeLambda xs exp)))
fixLambdas (Print exp)              =   (Print (fixLambdas exp))             
fixLambdas (EqEq exp1 exp2)         =   (EqEq (fixLambdas exp1) (fixLambdas exp2))
fixLambdas (Neq exp1 exp2)          =   (Neq (fixLambdas exp1) (fixLambdas exp2))
fixLambdas (LtLt exp1 exp2)         =   (LtLt (fixLambdas exp1) (fixLambdas exp2))
fixLambdas (Leq exp1 exp2)          =   (Leq (fixLambdas exp1) (fixLambdas exp2))
fixLambdas (GtGt exp1 exp2)         =   (GtGt (fixLambdas exp1) (fixLambdas exp2))
fixLambdas (Geq exp1 exp2)          =   (Geq (fixLambdas exp1) (fixLambdas exp2))
fixLambdas (And exp1 exp2)          =   (And (fixLambdas exp1) (fixLambdas exp2))
fixLambdas (Or exp1 exp2)           =   (Or (fixLambdas exp1) (fixLambdas exp2))
fixLambdas (Plus exp1 exp2)         =   (Plus (fixLambdas exp1) (fixLambdas exp2))
fixLambdas (Minus exp1 exp2)        =   (Minus (fixLambdas exp1) (fixLambdas exp2))
fixLambdas (Times exp1 exp2)        =   (Times (fixLambdas exp1) (fixLambdas exp2))
fixLambdas (Divide exp1 exp2)       =   (Divide (fixLambdas exp1) (fixLambdas exp2))
fixLambdas (Isnil exp)              =   (Isnil (fixLambdas exp))
fixLambdas (Cons exp1 exp2)         =   (Cons (fixLambdas exp1) (fixLambdas exp2))
fixLambdas (HD exp)                 =   (HD (fixLambdas exp))
fixLambdas (TL exp)                 =   (TL (fixLambdas exp))
fixLambdas (Branch exp1 exp2 exp3)  =   (Branch (fixLambdas exp1) (fixLambdas exp2) (fixLambdas exp3))
fixLambdas (Let id exp1 exp2)       =   (Let id (fixLambdas exp1) (fixLambdas exp2))
fixLambdas (Application xs)         =   (Application (map fixLambdas xs))
fixLambdas exp                      =   exp

subst :: IdConst -> Exp -> Exp -> Exp
subst old sub (Print exp)               =   (Print (subst old sub exp))
subst old sub (EqEq exp1 exp2)          =   (EqEq (subst old sub exp1) (subst old sub exp2))
subst old sub (Neq exp1 exp2)           =   (Neq (subst old sub exp1) (subst old sub exp2))
subst old sub (LtLt exp1 exp2)          =   (LtLt (subst old sub exp1) (subst old sub exp2))
subst old sub (Leq exp1 exp2)           =   (Leq (subst old sub exp1) (subst old sub exp2))
subst old sub (GtGt exp1 exp2)          =   (GtGt (subst old sub exp1) (subst old sub exp2))
subst old sub (Geq exp1 exp2)           =   (Geq (subst old sub exp1) (subst old sub exp2))
subst old sub (And exp1 exp2)           =   (And (subst old sub exp1) (subst old sub exp2))
subst old sub (Or exp1 exp2)            =   (Or (subst old sub exp1) (subst old sub exp2))
subst old sub (Plus exp1 exp2)          =   (Plus (subst old sub exp1) (subst old sub exp2))
subst old sub (Minus exp1 exp2)         =   (Minus (subst old sub exp1) (subst old sub exp2))
subst old sub (Times exp1 exp2)         =   (Times (subst old sub exp1) (subst old sub exp2))
subst old sub (Divide exp1 exp2)        =   (Divide (subst old sub exp1) (subst old sub exp2))
subst old sub (Isnil exp)               =   (Isnil (subst old sub exp))
subst old sub (Cons exp1 exp2)          =   (Cons (subst old sub exp1) (subst old sub exp2))
subst old sub (HD exp)                  =   (HD (subst old sub exp))
subst old sub (TL exp)                  =   (TL (subst old sub exp))
subst old sub (Branch exp1 exp2 exp3)   =   (Branch (subst old sub exp1) (subst old sub exp2) (subst old sub exp3))
subst old sub (Let id exp1 exp2)        =   (Let id (subst old sub exp1) (subst old sub exp2))
subst old sub (Application xs)          =   (Application (map (subst old sub) xs))
subst old sub (Lambda idConst exp)
    |   idConst == old                  =   (Lambda idConst exp)
    |   otherwise                       =   (Lambda idConst (subst old sub exp))
subst old sub (ExpId idConst)
    |   idConst == old                  =   sub
    |   otherwise                       =   (ExpId idConst)
subst old sub exp                       =   exp
