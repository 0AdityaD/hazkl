import Lexer
import Parser
import Grammar
import Data.Char

main :: IO ()
main = do program <- getContents
          let result = parse . map toLower $ program
          case result of
            Left err -> print err
            Right ok -> print . fixLambdas $ ok
