import Text.ParserCombinators.Parsec
import System.Environment

-- Each parser takes in a sequence of Chars and returns either
--  * an array of Strings ([String])
--  * an array of arrays of Strings ([String])

csvFile :: GenParser Char a [[String]]
-- A csv file contains 0 or more lines and is terminated by the end of the file
csvFile = do
  result <- many line -- The result is an array of an array of Strings, i.e. [[String]]
  eof -- match end of file
  return result

line :: GenParser Char a [String]
-- A line is a comma separated list of values, ending with a newline character
line = do
  result <- many cell
  try ((char '\n') >> (return result)) <?> (concat result) -- end of line, only works in Linux or OSX formatted files
  -- return result

cell :: GenParser Char a String
cell = do
  tokens <- many (noneOf ",\n")
  try ((char ',') >> (return tokens)) <?> tokens
  -- return tokens


-- cells :: GenParser Char st [String]
-- cells = do
--   first <- cellContent
--   next <- remainingCells
--   return (first : next)

-- remainingCells :: GenParser Char st [String]
-- remainingCells = do
--   (char ',' >> cells) -- found a comma, so more cells
--   <|> (return []) -- No comma, so we wrap up an empty array

-- cellContent :: GenParser Char st String
-- cellContent =
--   many (noneOf ",\n") -- Matches any character that is not a comma or newline

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

main = do
  args <- getArgs
  p <- parseFromFile csvFile (head args)
  case p of
    Left err  -> print err
    Right csv -> print csv

