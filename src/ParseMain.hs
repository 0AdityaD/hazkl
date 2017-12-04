import Lexer
import Parser
import Grammar
import Token
import Data.Char

genAst :: String -> Exp
genAst = fixLambdas . parse . scanTokens . map toLower

main :: IO ()
main = do   program <- getContents
            putStrLn . show . genAst $ program
