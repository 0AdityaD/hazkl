import Token
import Lexer
import Grammar
import Parser
import System.IO
import System.Environment
import Data.Char
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

data Prog   =   LambdaVal IdConst Exp
            |   StringVal String
            |   IntVal Int
        deriving (Show, Eq)

type Env    =   Map IdConst Prog

progToExp :: Prog -> Exp
progToExp (IntVal i)                    =   ExpInt (Int i)
progToExp (StringVal s)                 =   ExpString (String s)
progToExp (LambdaVal idConst exp)       =   Lambda idConst exp

expToProg :: Exp -> Prog
expToProg (ExpInt (Int i))              =   IntVal i
expToProg (ExpString (String str))      =   StringVal str
expToProg (Lambda idConst exp)          =   LambdaVal idConst exp

printProg :: Prog -> IO Prog
printProg p = do    putStrLn . show $ p
                    return (IntVal 0)

getNum :: IO Int
getNum = do line <- getLine
            if and . map isDigit $ line then
                return (read line :: Int)
            else
                return (0)

readIntProg :: IO Prog
readIntProg = do    num <- getNum
                    return (IntVal num)

readStringProg :: IO Prog
readStringProg = do string <- getLine
                    return (StringVal string)

apply :: Exp -> [Exp] -> Exp
apply exp []                        =   exp
apply (Lambda idConst exp1) (x:xs)  =   subst idConst x (apply exp1 xs)

eval :: Env -> Exp -> IO Prog
eval _ (ExpInt (Int i))                                         =   return (IntVal i)

eval _ (ExpString (String str))                                 =   return (StringVal str)

eval s (Lambda idConst exp1)                                    =   return (LambdaVal (idConst) exp1)

eval s (ExpId idConst)                                          =   return (fromJust (Map.lookup idConst s))

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

eval _ (ReadInt)                                                =   readIntProg

eval _ (ReadString)                                             =   readStringProg

eval s (Branch (ExpInt (Int i)) exp2 exp3)                      =   if i /= 0 then
                                                                        eval s exp2
                                                                    else
                                                                        eval s exp3
eval s (Branch exp1 exp2 exp3)                                  =   do  p1 <- eval s exp1
                                                                        let e1 = progToExp p1
                                                                        eval s (Branch e1 exp2 exp3)

eval s (Let idConst exp1 exp2)                                  =   do  p1 <- eval s exp1
                                                                        let s' = Map.insert idConst p1 s
                                                                        eval s' exp2

eval s (Application (x:xs))                                     =   eval s (apply x xs)

-- TODO: ISNIL, CONS, HD, TL, NIL, Application

main :: IO ()
main =  do  args <- getArgs
            let filename = head args
            handle <- openFile filename ReadMode
            program <- hGetContents handle
            result <- eval (Map.fromList []) . fixLambdas . parse . scanTokens . map toLower $ program
            putStrLn . show $ result