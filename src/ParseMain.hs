import Lexer
import Parser
import Grammar
import Token

genAst :: String -> Exp
genAst = fixLambdas . parse . scanTokens

main :: IO ()
main = do   program <- getContents
            putStrLn . show . genAst $ program
