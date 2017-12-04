import System.IO
import Data.Char
import Lexer

main :: IO ()
main = do
    code <- getContents
    let tokens = scanTokens . map toLower $ code
    putStrLn . unlines . map show $ tokens
