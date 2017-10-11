import Text.ParserCombinators.Parsec
import System.Environment

num :: GenParser Char st Integer
num = do
   str <- many1 digit
   return $ read str
main = do
   print $ parse num "example 2" "2"