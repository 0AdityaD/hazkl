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
printExp e = do print e
                return (ExpInt (Int 0))

getNum :: IO Int
getNum = do line <- getLine
            if all isDigit line then
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
                                                case e1 of
                                                    (Lambda idConst exp1)   ->  apply s e1 (x:xs)
                                                    (_)                     ->  return (nonLambdaApplyError (x:xs))

binopSameTypeError :: Exp -> Exp
binopSameTypeError exp = Error ("Run-time error in expression " ++ showUnevaluatedCons exp ++ "\nBinop can only be applied to expressions of same type")

binopStringError :: String -> Exp -> Exp
binopStringError str exp = Error ("Run-time error in expression " ++ showUnevaluatedCons exp ++ "\nBinop " ++ str ++  " cannot be applied to strings")

missingIdentifierError :: IdConst -> Exp
missingIdentifierError id = Error ("Run-time error in expression " ++ show id ++ "\nIdentifier " ++ show id ++ " is not bound in current context")

nilConsError :: Exp -> Exp
nilConsError exp = Error ("Run-time error in expression " ++ showUnevaluatedCons exp ++ "\nNil can only be used with binop @")

listConsError :: Exp -> Exp
listConsError exp = Error ("Run-time error in expression " ++ showUnevaluatedCons exp ++ "\nBinop @ is the only legal binop for lists")

nonLambdaApplyError :: [Exp] -> Exp
nonLambdaApplyError exps = Error ("Run-time error in expression (" ++ showApp exps ++ ")\nOnly lambda expressions can be applied to other expressions")

condNotIntError :: Exp -> Exp
condNotIntError exp = Error ("Run-time error in expression " ++ showUnevaluatedCons exp ++ "\nPredicate in conditional must be an integer") 

eval :: Env -> Exp -> IO Exp
eval _ (ExpInt (Int i))                                         =   return (ExpInt (Int i))

eval _ (ExpString (String str))                                 =   return (ExpString (String str))

eval s (Lambda idConst exp1)                                    =   return (Lambda (idConst) exp1)

eval s (ExpId idConst)                                          =   do  let value = Map.lookup idConst s
                                                                        case value of
                                                                            (Nothing)   ->  return (missingIdentifierError idConst)
                                                                            (Just x)    ->  return (x)

eval s (Print (Error str))                                      =   return (Error str)
eval s (Print (ExpInt (Int i)))                                 =   do  result <- (printExp (ExpInt (Int i)))
                                                                        return result
eval s (Print (ExpString (String str)))                         =   do  result <- (printExp (ExpString (String str)))
                                                                        return result
eval s (Print (Lambda idConst exp1))                            =   do  result <- (printExp (Lambda idConst exp1))
                                                                        return result
eval s (Print exp)                                              =   do  e1 <- eval s exp
                                                                        eval s (Print e1)

eval s (EqEq exp1 exp2)     =   do  e1 <- eval s exp1
                                    case e1 of
                                        (Error _)                                       -> return (e1)
                                        otherwise                                       -> do
                                            e2 <- eval s exp2
                                            case (e1, e2) of
                                                (ExpInt (Int i), ExpInt (Int j))                -> return (ExpInt (Int (if test then 1 else 0)))
                                                    where test = (i == j)
                                                (ExpString (String i), ExpString (String j))    -> return (ExpInt (Int (if test then 1 else 0)))
                                                    where test = (i == j)
                                                (Nil, _)                                        -> return (nilConsError (EqEq exp1 exp2))
                                                (_, Nil)                                        -> return (nilConsError (EqEq exp1 exp2))
                                                ((Cons _ _), _)                                 -> return (listConsError (EqEq exp1 exp2))
                                                (_, (Cons _ _))                                 -> return (listConsError (EqEq exp1 exp2))
                                                (_, (Error str))                                -> return (Error str)
                                                (_, _)                                          -> return (binopSameTypeError (EqEq exp1 exp2))

eval s (Neq exp1 exp2)      =   do  e1 <- eval s exp1
                                    case e1 of
                                        (Error _)                                       -> return (e1)
                                        otherwise                                       -> do
                                            e2 <- eval s exp2
                                            case (e1, e2) of
                                                (ExpInt (Int i), ExpInt (Int j))                -> return (ExpInt (Int (if test then 1 else 0)))
                                                    where test = (i /= j)
                                                (ExpString (String i), ExpString (String j))    -> return (ExpInt (Int (if test then 1 else 0)))
                                                    where test = (i /= j)
                                                (Nil, _)                                        -> return (nilConsError (Neq exp1 exp2))
                                                (_, Nil)                                        -> return (nilConsError (Neq exp1 exp2))
                                                ((Cons _ _), _)                                 -> return (listConsError (Neq exp1 exp2))
                                                (_, (Cons _ _))                                 -> return (listConsError (Neq exp1 exp2))
                                                (_, (Error str))                                -> return (Error str)
                                                (_, _)                                          -> return (binopSameTypeError (Neq exp1 exp2))

eval s (LtLt exp1 exp2)     =   do  e1 <- eval s exp1
                                    case e1 of
                                        (Error _)                                       -> return (e1)
                                        otherwise                                       -> do
                                            e2 <- eval s exp2
                                            case (e1, e2) of
                                                (ExpInt (Int i), ExpInt (Int j))                -> return (ExpInt (Int (if test then 1 else 0)))
                                                    where test = (i < j)
                                                (ExpString (String i), ExpString (String j))    -> return (binopStringError "<" (LtLt exp1 exp2))
                                                (Nil, _)                                        -> return (nilConsError (LtLt exp1 exp2))
                                                (_, Nil)                                        -> return (nilConsError (LtLt exp1 exp2))
                                                ((Cons _ _), _)                                 -> return (listConsError (LtLt exp1 exp2))
                                                (_, (Cons _ _))                                 -> return (listConsError (LtLt exp1 exp2))
                                                (_, (Error str))                                -> return (Error str)
                                                (_, _)                                          -> return (binopSameTypeError (LtLt exp1 exp2))

eval s (Leq exp1 exp2)      =   do  e1 <- eval s exp1
                                    case e1 of
                                        (Error _)                                       -> return (e1)
                                        otherwise                                       -> do
                                            e2 <- eval s exp2
                                            case (e1, e2) of
                                                (ExpInt (Int i), ExpInt (Int j))                -> return (ExpInt (Int (if test then 1 else 0)))
                                                    where test = (i <= j)
                                                (ExpString (String i), ExpString (String j))    -> return (binopStringError "<=" (Leq exp1 exp2))
                                                (Nil, _)                                        -> return (nilConsError (Leq exp1 exp2))
                                                (_, Nil)                                        -> return (nilConsError (Leq exp1 exp2))
                                                ((Cons _ _), _)                                 -> return (listConsError (Leq exp1 exp2))
                                                (_, (Cons _ _))                                 -> return (listConsError (Leq exp1 exp2))
                                                (_, (Error str))                                -> return (Error str)
                                                (_, _)                                          -> return (binopSameTypeError (Leq exp1 exp2))

eval s (GtGt exp1 exp2)     =   do  e1 <- eval s exp1
                                    case e1 of
                                        (Error _)                                       -> return (e1)
                                        otherwise                                       -> do
                                            e2 <- eval s exp2
                                            case (e1, e2) of
                                                (ExpInt (Int i), ExpInt (Int j))                -> return (ExpInt (Int (if test then 1 else 0)))
                                                    where test = (i > j)
                                                (ExpString (String i), ExpString (String j))    -> return (binopStringError ">" (GtGt exp1 exp2))
                                                (Nil, _)                                        -> return (nilConsError (GtGt exp1 exp2))
                                                (_, Nil)                                        -> return (nilConsError (GtGt exp1 exp2))
                                                ((Cons _ _), _)                                 -> return (listConsError (GtGt exp1 exp2))
                                                (_, (Cons _ _))                                 -> return (listConsError (GtGt exp1 exp2))
                                                (_, (Error str))                                -> return (Error str)
                                                (_, _)                                          -> return (binopSameTypeError (GtGt exp1 exp2))

eval s (Geq exp1 exp2)      =   do  e1 <- eval s exp1
                                    case e1 of
                                        (Error _)                                       -> return (e1)
                                        otherwise                                       -> do
                                            e2 <- eval s exp2
                                            case (e1, e2) of
                                                (ExpInt (Int i), ExpInt (Int j))                -> return (ExpInt (Int (if test then 1 else 0)))
                                                    where test = (i >= j)
                                                (ExpString (String i), ExpString (String j))    -> return (binopStringError ">=" (Geq exp1 exp2))
                                                (Nil, _)                                        -> return (nilConsError (Geq exp1 exp2))
                                                (_, Nil)                                        -> return (nilConsError (Geq exp1 exp2))
                                                ((Cons _ _), _)                                 -> return (listConsError (Geq exp1 exp2))
                                                (_, (Cons _ _))                                 -> return (listConsError (Geq exp1 exp2))
                                                (_, (Error str))                                -> return (Error str)
                                                (_, _)                                          -> return (binopSameTypeError (Geq exp1 exp2))


eval s (And exp1 exp2)      =   do  e1 <- eval s exp1
                                    case e1 of
                                        (Error _)                                       -> return (e1)
                                        otherwise                                       -> do
                                            e2 <- eval s exp2
                                            case (e1, e2) of
                                                (ExpInt (Int i), ExpInt (Int j))                -> return (ExpInt (Int (if test then 1 else 0)))
                                                    where test = (i /= 0) && (j /= 0)
                                                (ExpString (String i), ExpString (String j))    -> return (binopStringError "&" (And exp1 exp2))
                                                (Nil, _)                                        -> return (nilConsError (And exp1 exp2))
                                                (_, Nil)                                        -> return (nilConsError (And exp1 exp2))
                                                ((Cons _ _), _)                                 -> return (listConsError (And exp1 exp2))
                                                (_, (Cons _ _))                                 -> return (listConsError (And exp1 exp2))
                                                (_, (Error str))                                -> return (Error str)
                                                (_, _)                                          -> return (binopSameTypeError (And exp1 exp2))

eval s (Or exp1 exp2)       =   do  e1 <- eval s exp1
                                    case e1 of
                                        (Error _)                                       -> return (e1)
                                        otherwise                                       -> do
                                            e2 <- eval s exp2
                                            case (e1, e2) of
                                                (ExpInt (Int i), ExpInt (Int j))                -> return (ExpInt (Int (if test then 1 else 0)))
                                                    where test = (i /= 0) || (j /= 0)
                                                (ExpString (String i), ExpString (String j))    -> return (binopStringError "|" (Or exp1 exp2))
                                                (Nil, _)                                        -> return (nilConsError (Or exp1 exp2))
                                                (_, Nil)                                        -> return (nilConsError (Or exp1 exp2))
                                                ((Cons _ _), _)                                 -> return (listConsError (Or exp1 exp2))
                                                (_, (Cons _ _))                                 -> return (listConsError (Or exp1 exp2))
                                                (_, (Error str))                                -> return (Error str)
                                                (_, _)                                          -> return (binopSameTypeError (Or exp1 exp2))

eval s (Plus exp1 exp2)     =   do  e1 <- eval s exp1
                                    case e1 of
                                        (Error _)                                       -> return (e1)
                                        otherwise                                       -> do
                                            e2 <- eval s exp2
                                            case (e1, e2) of
                                                (ExpInt (Int i), ExpInt (Int j))                -> return (ExpInt (Int (i + j)))
                                                (ExpString (String i), ExpString (String j))    -> return (ExpString (String (i ++ j)))
                                                (Nil, _)                                        -> return (nilConsError (Plus exp1 exp2))
                                                (_, Nil)                                        -> return (nilConsError (Plus exp1 exp2))
                                                ((Cons _ _), _)                                 -> return (listConsError (Plus exp1 exp2))
                                                (_, (Cons _ _))                                 -> return (listConsError (Plus exp1 exp2))
                                                (_, (Error str))                                -> return (Error str)
                                                (_, _)                                          -> return (binopSameTypeError (Plus exp1 exp2))

eval s (Minus exp1 exp2)    =   do  e1 <- eval s exp1
                                    case e1 of
                                        (Error _)                                       -> return (e1)
                                        otherwise                                       -> do
                                            e2 <- eval s exp2
                                            case (e1, e2) of
                                                (ExpInt (Int i), ExpInt (Int j))                -> return (ExpInt (Int (i - j)))
                                                (ExpString (String i), ExpString (String j))    -> return (binopStringError "-" (Minus exp1 exp2))
                                                (Nil, _)                                        -> return (nilConsError (Minus exp1 exp2))
                                                (_, Nil)                                        -> return (nilConsError (Minus exp1 exp2))
                                                ((Cons _ _), _)                                 -> return (listConsError (Minus exp1 exp2))
                                                (_, (Cons _ _))                                 -> return (listConsError (Minus exp1 exp2))
                                                (_, (Error str))                                -> return (Error str)
                                                (_, _)                                          -> return (binopSameTypeError (Minus exp1 exp2))

eval s (Times exp1 exp2)    =   do  e1 <- eval s exp1
                                    case e1 of
                                        (Error _)                                       -> return (e1)
                                        otherwise                                       -> do
                                            e2 <- eval s exp2
                                            case (e1, e2) of
                                                (ExpInt (Int i), ExpInt (Int j))                -> return (ExpInt (Int (i * j)))
                                                (ExpString (String i), ExpString (String j))    -> return (binopStringError "*" (Times exp1 exp2))
                                                (Nil, _)                                        -> return (nilConsError (Times exp1 exp2))
                                                (_, Nil)                                        -> return (nilConsError (Times exp1 exp2))
                                                ((Cons _ _), _)                                 -> return (listConsError (Times exp1 exp2))
                                                (_, (Cons _ _))                                 -> return (listConsError (Times exp1 exp2))
                                                (_, (Error str))                                -> return (Error str)
                                                (_, _)                                          -> return (binopSameTypeError (Times exp1 exp2))

eval s (Divide exp1 exp2)   =   do  e1 <- eval s exp1
                                    case e1 of
                                        (Error _)                                       -> return (e1)
                                        otherwise                                       -> do
                                            e2 <- eval s exp2
                                            case (e1, e2) of
                                                (ExpInt (Int i), ExpInt (Int j))                -> return (ExpInt (Int (i `div` j)))
                                                (ExpString (String i), ExpString (String j))    -> return (binopStringError "/" (Divide exp1 exp2))
                                                (Nil, _)                                        -> return (nilConsError (Divide exp1 exp2))
                                                (_, Nil)                                        -> return (nilConsError (Divide exp1 exp2))
                                                ((Cons _ _), _)                                 -> return (listConsError (Divide exp1 exp2))
                                                (_, (Cons _ _))                                 -> return (listConsError (Divide exp1 exp2))
                                                (_, (Error str))                                -> return (Error str)
                                                (_, _)                                          -> return (binopSameTypeError (Divide exp1 exp2))

eval _ (ReadInt)            =   readIntExp

eval _ (ReadString)         =   readStringExp

eval s (Branch exp1 exp2 exp3)  =   do  e1 <- eval s exp1
                                        case e1 of
                                            (Error _)           ->  return (e1)
                                            (ExpInt (Int i))    ->  do  if i /= 0 then
                                                                            eval s exp2
                                                                        else
                                                                            eval s exp3
                                            (_)                 ->  return (condNotIntError (Branch exp1 exp2 exp3))

eval s (Let idConst exp1 exp2)  =   do  e1 <- eval s exp1
                                        case e1 of
                                            (Error _)           -> return (e1)
                                            otherwise          -> do
                                                let s' = Map.insert idConst e1 s
                                                eval s' exp2

eval s (Application (x:xs))     =   do  e1 <- apply s x xs
                                        case e1 of
                                            Error _     -> return (e1)
                                            (_)         -> eval s e1 

eval s (Nil)                    =   return (Nil)

eval s (Cons exp1 exp2)         =   do  e1 <- eval s exp1
                                        case e1 of
                                            (Error _)           -> return (e1)
                                            otherwise           -> do
                                                e2 <- eval s exp2
                                                case e2 of
                                                    (Error _)   -> return (e2)
                                                    otherwise   -> return (Cons e1 e2)

eval s (HD (Error str))                     =   return (Error str)
eval s (HD (Nil))                           =   return (Nil)
eval s (HD (Cons exp1 exp2))                =   return (exp1)
eval s (HD (ExpInt i))                      =   return (ExpInt i)
eval s (HD (ExpString str))                 =   return (ExpString str)
eval s (HD (Lambda idConst exp))            =   return (Lambda idConst exp)
eval s (HD exp1)                            =   do  e1 <- eval s exp1
                                                    eval s (HD e1)

eval s (TL (Error str))                     =   return (Error str)
eval s (TL (Nil))                           =   return (Nil)
eval s (TL (Cons exp1 exp2))                =   return (exp2)
eval s (TL (ExpInt i))                      =   return (Nil)
eval s (TL (ExpString str))                 =   return (Nil)
eval s (TL (Lambda idConst exp))            =   return (Nil)
eval s (TL exp1)                            =   do  e1 <- eval s exp1
                                                    eval s (TL e1)

eval s (Isnil (Error str))                  =   return (Error str)
eval s (Isnil (Nil))                        =   return (ExpInt (Int 1))
eval s (Isnil (ExpInt _))                   =   return (ExpInt (Int 0))
eval s (Isnil (ExpString _))                =   return (ExpInt (Int 0))
eval s (Isnil (Lambda _ _))                 =   return (ExpInt (Int 0))
eval s (Isnil (Cons _ _))                   =   return (ExpInt (Int 0))
eval s (Isnil (exp))                        =   do  e1 <- eval s exp
                                                    eval s (Isnil (e1))


main :: IO ()
main =  do  args <- getArgs
            let filename = head args
            handle <- openFile filename ReadMode
            program <- hGetContents handle
            result <- eval (Map.fromList []) . fixLambdas . parse . scanTokens . map toLower $ program
            putStrLn . show $ result
