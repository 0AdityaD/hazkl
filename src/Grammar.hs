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
    deriving (Show, Eq)

data StringConst = String String
    deriving (Show, Eq)

data IdConst = Id String
    deriving (Show, Eq)

type IdList = [IdConst]

type App = [Exp]

fixLambdas :: Exp -> Exp
fixLambdas (FakeLambda [x] exp)     =   (Lambda x exp)
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
