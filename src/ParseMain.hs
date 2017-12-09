import Lexer
import Parser
import Grammar
import Data.Char

main :: IO ()
main = do program <- getContents
          let result = parse . progLower True $ program
          case result of
            Left err -> print err
            Right ok -> print . fixLambdas $ ok
