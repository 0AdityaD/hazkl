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

type Env    =   Map IdConst Exp

printExp :: Exp -> IO Exp
printExp e = do putStrLn . show $ e
                return (ExpInt (Int 0))

getNum :: IO Int
getNum = do line <- getLine
            if and . map isDigit $ line then
                return (read line :: Int)
            else
                return (0)

readIntExp :: IO Exp
readIntExp = do num <- getNum
                return (ExpInt (Int num))

readStringExp :: IO Exp
readStringExp = do  string <- getLine
                    return (ExpString (String string))

apply :: Env -> Exp -> [Exp] -> IO Exp
apply s (Lambda idConst exp1) (x:xs)    =   do  e1 <- (apply s exp1 xs)
                                                return (subst idConst x e1)
apply s exp []                          =   return exp
apply s exp (x:xs)                      =   do  e1 <- eval s exp
                                                apply s e1 (x:xs)

eval :: Env -> Exp -> IO Exp
eval _ (ExpInt (Int i))                                         =   return (ExpInt (Int i))

eval _ (ExpString (String str))                                 =   return (ExpString (String str))

eval s (Lambda idConst exp1)                                    =   return (Lambda (idConst) exp1)

eval s (ExpId idConst)                                          =   return (fromJust (Map.lookup idConst s))

eval s (Print (ExpInt (Int i)))                                 =   do  result <- (printExp (ExpInt (Int i)))
                                                                        return result
eval s (Print (ExpString (String str)))                         =   do  result <- (printExp (ExpString (String str)))
                                                                        return result
eval s (Print (Lambda idConst exp1))                            =   do  result <- (printExp (Lambda idConst exp1))
                                                                        return result
eval s (Print exp)                                              =   do  e1 <- eval s exp
                                                                        eval s (Print e1)
eval s (EqEq (ExpInt (Int i)) (ExpInt (Int j)))                 =   return (ExpInt (Int (if i == j then 1 else 0)))
eval s (EqEq exp1 exp2)                                         =   do  e1 <- eval s exp1
                                                                        e2 <- eval s exp2
                                                                        eval s (EqEq e1 e2)

eval s (Neq (ExpInt (Int i)) (ExpInt (Int j)))                  =   return (ExpInt (Int (if i /= j then 1 else 0)))
eval s (Neq exp1 exp2)                                          =   do  e1 <- eval s exp1
                                                                        e2 <- eval s exp2
                                                                        eval s (Neq e1 e2)

eval s (LtLt (ExpInt (Int i)) (ExpInt (Int j)))                 =   return (ExpInt (Int (if i < j then 1 else 0)))
eval s (LtLt exp1 exp2)                                         =   do  e1 <- eval s exp1
                                                                        e2 <- eval s exp2
                                                                        eval s (LtLt e1 e2)

eval s (Leq (ExpInt (Int i)) (ExpInt (Int j)))                  =   return (ExpInt (Int (if i <= j then 1 else 0)))
eval s (Leq exp1 exp2)                                          =   do  e1 <- eval s exp1
                                                                        e2 <- eval s exp2
                                                                        eval s (Leq e1 e2)

eval s (GtGt (ExpInt (Int i)) (ExpInt (Int j)))                 =   return (ExpInt (Int (if i > j then 1 else 0)))
eval s (GtGt exp1 exp2)                                         =   do  e1 <- eval s exp1
                                                                        e2 <- eval s exp2
                                                                        eval s (GtGt e1 e2)

eval s (Geq (ExpInt (Int i)) (ExpInt (Int j)))                  =   return (ExpInt (Int (if i >= j then 1 else 0)))
eval s (Geq exp1 exp2)                                          =   do  e1 <- eval s exp1
                                                                        e2 <- eval s exp2
                                                                        eval s (Geq e1 e2)

eval s (And (ExpInt (Int i)) (ExpInt (Int j)))                  =   return (ExpInt (Int (if k && l then 1 else 0)))
    where   k   =   i /= 0
            l   =   j /= 0
eval s (And exp1 exp2)                                          =   do  e1 <- eval s exp1
                                                                        e2 <- eval s exp2
                                                                        eval s (And e1 e2)

eval s (Or (ExpInt (Int i)) (ExpInt (Int j)))                   =   return (ExpInt (Int (if k || l then 1 else 0)))
    where   k   =   i /= 0
            l   =   j /= 0
eval s (Or exp1 exp2)                                           =   do  e1 <- eval s exp1
                                                                        e2 <- eval s exp2
                                                                        eval s (Or e1 e2)

eval s (Plus (ExpInt (Int i)) (ExpInt (Int j)))                 =   return (ExpInt (Int (i + j)))
eval s (Plus (ExpString (String s1)) (ExpString (String s2)))   =   return (ExpString (String (s1 ++ s2)))
eval s (Plus exp1 exp2)                                         =   do  e1 <- eval s exp1
                                                                        e2 <- eval s exp2
                                                                        eval s (Plus e1 e2)

eval s (Minus (ExpInt (Int i)) (ExpInt (Int j)))                =   return (ExpInt (Int (i - j)))
eval s (Minus exp1 exp2)                                        =   do  e1 <- eval s exp1
                                                                        e2 <- eval s exp2
                                                                        eval s (Minus e1 e2)

eval s (Times (ExpInt (Int i)) (ExpInt (Int j)))                =   return (ExpInt (Int (i * j)))
eval s (Times exp1 exp2)                                        =   do  e1 <- eval s exp1
                                                                        e2 <- eval s exp2
                                                                        eval s (Times e1 e2)

eval s (Divide (ExpInt (Int i)) (ExpInt (Int j)))               =   return (ExpInt (Int (i `div` j)))
eval s (Divide exp1 exp2)                                       =   do  e1 <- eval s exp1
                                                                        e2 <- eval s exp2
                                                                        eval s (Divide e1 e2)

eval _ (ReadInt)                                                =   readIntExp

eval _ (ReadString)                                             =   readStringExp

eval s (Branch (ExpInt (Int i)) exp2 exp3)                      =   if i /= 0 then
                                                                        eval s exp2
                                                                    else
                                                                        eval s exp3
eval s (Branch exp1 exp2 exp3)                                  =   do  e1 <- eval s exp1
                                                                        eval s (Branch e1 exp2 exp3)

eval s (Let idConst exp1 exp2)                                  =   do  e1 <- eval s exp1
                                                                        let s' = Map.insert idConst e1 s
                                                                        eval s' exp2

eval s (Application (x:xs))                                     =   do  e1 <- apply s x xs
                                                                        eval s e1 

eval s (Nil)                                                    =   return (Nil)

eval s (Cons exp1 exp2)                                         =   do  e1 <- eval s exp1
                                                                        e2 <- eval s exp2
                                                                        return (Cons e1 e2)

eval s (HD (Nil))                                               =   return (Nil)
eval s (HD (Cons exp1 exp2))                                    =   return (exp1)
eval s (HD exp1)                                                =   do  e1 <- eval s exp1
                                                                        eval s (HD e1)

eval s (TL (Nil))                                               =   return (Nil)
eval s (TL (Cons exp1 exp2))                                    =   return (exp2)
eval s (TL exp1)                                                =   do  e1 <- eval s exp1
                                                                        eval s (TL e1)

eval s (Isnil (Nil))                                            =   return (ExpInt (Int 1))
eval s (Isnil (ExpInt _))                                       =   return (ExpInt (Int 0))
eval s (Isnil (ExpString _))                                    =   return (ExpInt (Int 0))
eval s (Isnil (Lambda _ _))                                     =   return (ExpInt (Int 0))
eval s (Isnil (Cons _ _))                                       =   return (ExpInt (Int 0))
eval s (Isnil (exp))                                            =   do  e1 <- eval s exp
                                                                        eval s (Isnil (e1))


main :: IO ()
main =  do  args <- getArgs
            let filename = head args
            handle <- openFile filename ReadMode
            program <- hGetContents handle
            result <- eval (Map.fromList []) . fixLambdas . parse . scanTokens . map toLower $ program
            putStrLn . show $ result
