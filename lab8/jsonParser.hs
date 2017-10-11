import Text.ParserCombinators.Parsec
import System.Environment
import Data.List (intercalate)

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
  deriving (Eq, Ord)


-- instance Show JValue where
--   show (JString str) = "\"" ++ str ++ "\""
--   show (JNumber doub) = show doub
--   show (JBool True) = "true"
--   show (JBool False) = "false"
--   show (JArray lst) = "[\n" ++ items ++ "\n]"
--               where items = intercalate ",\n" (map show lst)
--   show (JNull) = "null"
--   show (JObject []) = "{}"
--   show (JObject lst) = "{\n" ++ items ++ "\n}"
--               where items = intercalate ",\n" ["\t" ++ key ++ " : " ++ value | x <- lst, let key = show (fst x), let value = show (snd x)]



prettify :: JValue -> String
prettify x = prettyPrint' 0 x


-- prettyPrint :: Integer -> JValue -> String
-- prettyPrint' indentLevel x = indents ++ (printVal x)
--               where indents = addTabs indentLevel

prettyPrint' :: Integer -> JValue -> String
prettyPrint' indentLevel (JString str) = (addTabs indentLevel) ++ "\"" ++ str ++ "\""
prettyPrint' indentLevel (JNumber doub) = (addTabs indentLevel) ++ show doub
prettyPrint' indentLevel (JBool True) = (addTabs indentLevel) ++ "true"
prettyPrint' indentLevel (JBool False) = (addTabs indentLevel) ++ "false"
prettyPrint' indentLevel (JArray lst) = (addTabs indentLevel) ++ "[\n" ++ items ++ "\n]"
             where newTabs = (addTabs indentLevel)
                   items = intercalate (",\n"++newTabs) (map prettify lst)
prettyPrint' indentLevel (JNull) = (addTabs indentLevel) ++ "null"
prettyPrint' indentLevel (JObject []) = (addTabs indentLevel) ++ "{}"
prettyPrint' indentLevel (JObject lst) = (addTabs indentLevel) ++ "{\n" ++ items ++ "\n}"
            where items = intercalate ",\n" ["\t" ++ key ++ " : " ++ value | x <- lst, let key = show (fst x), let value = prettify (snd x)]



addTabs :: Integer -> String
addTabs 0 = ""
addTabs x = "\t" ++ (addTabs (x-1))



jsonFile :: GenParser Char st JValue
jsonFile = do
  result <- jsonContainer
  spaces
  eof
  return result

jsonElem :: GenParser Char st JValue
jsonElem = do
  spaces
  result <- jsonElem'
  spaces
  return result


jsonContainer = jsonObject
        <|> jsonArr

jsonElem' = jsonContainer
        <|> jsonString
        <|> jsonBool
        <|> jsonNull
        <|> jsonNumber
        <?> "json element"

jsonString :: GenParser Char st JValue
jsonString = jsonStringDQ <|> jsonStringSQ

jsonStringDQ = do
  char '"'
  s <- many $ noneOf "\"" -- crude.  does not allow double quotes within strings
  char '"'
  return $ JString s

jsonStringSQ = do
  char '\''
  s <- many $ noneOf "'" -- crude, same as above
  char '\''
  return $ JString s

jsonBool = do
  bStr <- string "true" <|> string "false"
  return $ case bStr of
    "true" -> JBool True
    "false" -> JBool False

jsonNumber = do
  jNum <- many1 digit
  return $ JNumber (read jNum)

jsonObject = do
  char '{'
  items <- jsonObjectItem `sepBy` (char ',')
  char '}'
  return $ JObject items


jsonObjectItem = do
  spaces
  keyStr <- many $ noneOf ": "
  spaces
  char ':'
  valueStr <- jsonElem
  return $ (keyStr, valueStr)


jsonNull = do
  string "null"
  return JNull

jsonArr = do
  char '['
  arr <- jsonElem `sepBy` (char ',')
  char ']'
  return $ JArray arr



parseJSON :: String -> Either ParseError JValue
parseJSON input = parse jsonFile "(unknown)" input

main = do
  args <- getArgs
  p <- parseFromFile jsonFile (head args)
  case p of
    Left err  -> print err
    Right json -> putStrLn (prettify json)


