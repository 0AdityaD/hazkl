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
    deriving (Show, Eq)

data IntConst = Int Int
    deriving (Show, Eq, Ord)

data StringConst = String String
    deriving (Show, Eq, Ord)

data IdConst = Id String
    deriving (Show, Eq, Ord)

type IdList = [IdConst]

type App = [Exp]

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
subst old sub (ExpId i)
    |   old == i                        =   sub
    |   otherwise                       =   (ExpId i)
subst old sub exp                       =   exp
