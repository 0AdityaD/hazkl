import Token
import Lexer
import Grammar
import Parser
import Data.Char
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

printProg :: Prog -> IO Prog
printProg p = do    putStrLn . show $ p
                    return (IntVal 0)

eval :: Env -> Exp -> IO Prog
eval _ (ExpInt (Int i))                                         =   return (IntVal i)
eval _ (ExpString (String str))                                 =   return (StringVal str)
eval s (Lambda idConst exp1)                                    =   do  e1 <- eval s exp1
                                                                        return (LambdaVal (idConst) e1)

eval s (Print (ExpInt (Int i)))                                 =   do  result <- (printProg (IntVal i))
                                                                        return result
eval s (Print (ExpString (String str)))                         =   do  result <- (printProg (StringVal str))
                                                                        return result
eval s (Print (Lambda idConst exp1))                            =   do  e1 <- eval s exp1
                                                                        printProg e1
eval s (Print exp)                                              =   do  p1 <- eval s exp
                                                                        let e1 = progToExp p1
                                                                        eval s (Print e1)

eval s (EqEq (ExpInt (Int i)) (ExpInt (Int j)))                 =   return (IntVal (if i == j then 1 else 0))
eval s (EqEq exp1 exp2)                                         =   do  p1 <- eval s exp1
                                                                        let e1 = progToExp p1
                                                                        p2 <- eval s exp2
                                                                        let e2 = progToExp p2
                                                                        eval s (EqEq e1 e2)

eval s (Neq (ExpInt (Int i)) (ExpInt (Int j)))                  =   return (IntVal (if i /= j then 1 else 0))
eval s (Neq exp1 exp2)                                          =   do  p1 <- eval s exp1
                                                                        let e1 = progToExp p1
                                                                        p2 <- eval s exp2
                                                                        let e2 = progToExp p2
                                                                        eval s (Neq e1 e2)

eval s (LtLt (ExpInt (Int i)) (ExpInt (Int j)))                 =   return (IntVal (if i < j then 1 else 0))
eval s (LtLt exp1 exp2)                                         =   do  p1 <- eval s exp1
                                                                        let e1 = progToExp p1
                                                                        p2 <- eval s exp2
                                                                        let e2 = progToExp p2
                                                                        eval s (LtLt e1 e2)

eval s (Leq (ExpInt (Int i)) (ExpInt (Int j)))                  =   return (IntVal (if i <= j then 1 else 0))
eval s (Leq exp1 exp2)                                          =   do  p1 <- eval s exp1
                                                                        let e1 = progToExp p1
                                                                        p2 <- eval s exp2
                                                                        let e2 = progToExp p2
                                                                        eval s (Leq e1 e2)

eval s (GtGt (ExpInt (Int i)) (ExpInt (Int j)))                 =   return (IntVal (if i > j then 1 else 0))
eval s (GtGt exp1 exp2)                                         =   do  p1 <- eval s exp1
                                                                        let e1 = progToExp p1
                                                                        p2 <- eval s exp2
                                                                        let e2 = progToExp p2
                                                                        eval s (GtGt e1 e2)

eval s (Geq (ExpInt (Int i)) (ExpInt (Int j)))                  =   return (IntVal (if i >= j then 1 else 0))
eval s (Geq exp1 exp2)                                          =   do  p1 <- eval s exp1
                                                                        let e1 = progToExp p1
                                                                        p2 <- eval s exp2
                                                                        let e2 = progToExp p2
                                                                        eval s (Geq e1 e2)

eval s (And (ExpInt (Int i)) (ExpInt (Int j)))                  =   return (IntVal (if k && l then 1 else 0))
    where   k   =   i /= 0
            l   =   j /= 0
eval s (And exp1 exp2)                                          =   do  p1 <- eval s exp1
                                                                        let e1 = progToExp p1
                                                                        p2 <- eval s exp2
                                                                        let e2 = progToExp p2
                                                                        eval s (And e1 e2)

eval s (Or (ExpInt (Int i)) (ExpInt (Int j)))                   =   return (IntVal (if k || l then 1 else 0))
    where   k   =   i /= 0
            l   =   j /= 0
eval s (Or exp1 exp2)                                           =   do  p1 <- eval s exp1
                                                                        let e1 = progToExp p1
                                                                        p2 <- eval s exp2
                                                                        let e2 = progToExp p2
                                                                        eval s (Or e1 e2)

eval s (Plus (ExpInt (Int i)) (ExpInt (Int j)))                 =   return (IntVal (i + j))
eval s (Plus (ExpString (String s1)) (ExpString (String s2)))   =   return (StringVal (s1 ++ s2))
eval s (Plus exp1 exp2)                                         =   do  p1 <- eval s exp1
                                                                        let e1 = progToExp p1
                                                                        p2 <- eval s exp2
                                                                        let e2 = progToExp p2
                                                                        eval s (Plus e1 e2)

eval s (Minus (ExpInt (Int i)) (ExpInt (Int j)))                =   return (IntVal (i - j))
eval s (Minus exp1 exp2)                                        =   do  p1 <- eval s exp1
                                                                        let e1 = progToExp p1
                                                                        p2 <- eval s exp2
                                                                        let e2 = progToExp p2
                                                                        eval s (Minus e1 e2)

eval s (Times (ExpInt (Int i)) (ExpInt (Int j)))                =   return (IntVal (i * j))
eval s (Times exp1 exp2)                                        =   do  p1 <- eval s exp1
                                                                        let e1 = progToExp p1
                                                                        p2 <- eval s exp2
                                                                        let e2 = progToExp p2
                                                                        eval s (Times e1 e2)

eval s (Divide (ExpInt (Int i)) (ExpInt (Int j)))               =   return (IntVal (i `div` j))
eval s (Divide exp1 exp2)                                       =   do  p1 <- eval s exp1
                                                                        let e1 = progToExp p1
                                                                        p2 <- eval s exp2
                                                                        let e2 = progToExp p2
                                                                        eval s (Divide e1 e2)


main :: IO ()
main =  do  program <- getContents
            result <- eval (Map.fromList []) . fixLambdas . parse . scanTokens $ program
            putStrLn . show $ result
