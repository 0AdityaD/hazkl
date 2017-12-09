import System.IO
import Data.Char
import Lexer

main :: IO ()
main = do code <- getContents
          let tokens = scanner . progLower True $ code
          case tokens of
              Left err -> error err
              Right ok -> putStrLn . unlines . map show $ (map tokClass ok)
