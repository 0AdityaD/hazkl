import System.IO
import Lexer

main :: IO ()
main = do
    code <- getContents
    let tokens = scanTokens code
    putStrLn . show $ tokens
