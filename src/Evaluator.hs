import Token
import Lexer
import Grammar
import Parser
import Data.Map (Map)
import qualified Data.Map as Map


data Prog   =   LambdaVal IdConst Prog
            |   StringVal String
            |   IntVal Int
        deriving (Show, Eq)


type Env    =   Map IdConst Prog

progToExp :: Prog -> Exp
progToExp (IntVal i)                    =   ExpInt (Int i)
progToExp (StringVal s)                 =   ExpString (String s)
progToExp (LambdaVal idConst prog)      =   Lambda idConst (progToExp prog)

eval :: Env -> Exp -> Prog
eval _ (ExpInt (Int i))                                         =   IntVal i
eval _ (ExpString (String str))                                 =   StringVal str
eval e (Lambda idConst exp2)                                    =   LambdaVal (idConst) (eval e exp2)
eval e (Plus (ExpInt (Int i)) (ExpInt (Int j)))                 =   IntVal (i + j)
eval e (Plus (ExpString (String s1)) (ExpString (String s2)))   =   StringVal (s1 ++ s2)
eval e (Plus exp1 exp2)                                         =   eval e (Plus e1 e2)
    where   e1  =   progToExp (eval e exp1)
            e2  =   progToExp (eval e exp2)
eval e (Minus (ExpInt (Int i)) (ExpInt (Int j)))                =   IntVal (i - j)
eval e (Minus exp1 exp2)                                        =   eval e (Minus e1 e2)
    where   e1  =   progToExp (eval e exp1)
            e2  =   progToExp (eval e exp2)
eval e (Times (ExpInt (Int i)) (ExpInt (Int j)))                =   IntVal (i * j)
eval e (Times exp1 exp2)                                        =   eval e (Minus e1 e2)
    where   e1  =   progToExp (eval e exp1)
            e2  =   progToExp (eval e exp2)
eval e (Divide (ExpInt (Int i)) (ExpInt (Int j)))               =   IntVal (i `div` j)
eval e (Divide exp1 exp2)                                       =   eval e (Minus e1 e2)
    where   e1  =   progToExp (eval e exp1)
            e2  =   progToExp (eval e exp2)





main :: IO ()
main =  do  program <- getContents
            let result = eval (Map.fromList []) . fixLambdas . parse . scanTokens $ program
            putStrLn . show $ result
