import Text.ParserCombinators.Parsec
import System.Environment

-- Each parser takes in a sequence of Chars and returns either
--  * an array of Strings ([String])
--  * an array of arrays of Strings ([String])

csvFile :: GenParser Char st [[String]]
csvFile = line `endBy` eol -- a series of lines ended by the end of the file

line :: GenParser Char st [String]
line = cell `sepBy` (char ',') -- a series of cells separated by commas

cell :: GenParser Char st String
cell = many (noneOf ",\n")

eol :: GenParser Char st String
{-
eol = char '\n' -- does not work on Windows
eol = string "\n" <|> string "\n\r" -- always matches first pattern
eol = string "\n\r" <|> string "\n" -- fails on non-windows, since the left hand tries and consumes the \n
eol = do -- works, but ugly, and changes the types
  char '\n'
  char '\r' <|> return '\n'
-}
eol = try (string "\n\r")
       <|> string "\n"
       <?> "end of line"


parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

main = do
  args <- getArgs
  p <- parseFromFile csvFile (head args)
  case p of
    Left err  -> print err
    Right csv -> print csv

