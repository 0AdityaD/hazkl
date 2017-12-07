import System.IO
import Data.Char
import Lexer

main :: IO ()
main = do code <- getContents
          let tokens = scanner . map toLower $ code
          case tokens of
              Left err -> error err
              Right ok -> putStrLn . unlines . map show $ (map tokClass ok)
