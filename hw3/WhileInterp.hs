{-
  Name: Ujjawal Garg
  Class: CS 252
  Assigment: HW3
  Date: 16 October 2017
  Description: 
-}


module WhileInterp (
  Expression(..),
  Binop(..),
  Value(..),
  runFile,
  showParsedExp,
  run
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Control.Monad.Except

-- We represent variables as strings.
type Variable = String

--We also represent error messages as strings.
type ErrorMsg = String

-- The store is an associative map from variables to values.
-- (The store roughly corresponds with the heap in a language like Java).
type Store = Map Variable Value

data Expression =
    Var Variable                            -- x
  | Val Value                               -- v
  | Assign Variable Expression              -- x := e
  | Sequence Expression Expression          -- e1; e2
  | Op Binop Expression Expression
  | If Expression Expression Expression     -- if e1 then e2 else e3 endif
  | While Expression Expression             -- while e1 do e2 endwhile
  deriving (Show)

data Binop =
    Plus     -- +  :: Int  -> Int  -> Int
  | Minus    -- -  :: Int  -> Int  -> Int
  | Times    -- *  :: Int  -> Int  -> Int
  | Divide   -- /  :: Int  -> Int  -> Int
  | Gt       -- >  :: Int -> Int -> Bool
  | Ge       -- >= :: Int -> Int -> Bool
  | Lt       -- <  :: Int -> Int -> Bool
  | Le       -- <= :: Int -> Int -> Bool
  deriving (Show)

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show)


fileP :: GenParser Char st Expression
fileP = do
  prog <- exprP
  eof
  return prog

exprP = do
  e <- exprP'
  rest <- optionMaybe restSeqP
  return (case rest of
    Nothing   -> e
    Just e' -> Sequence e e')

-- Expressions are divided into terms and expressions for the sake of
-- parsing.  Note that binary operators **DO NOT** follow the expected
-- presidence rules.
--
-- ***FOR 2pts EXTRA CREDIT (hard, no partial credit)***
-- Correct the precedence of the binary operators.
exprP' = do
  spaces
  t <- termP
  spaces
  rest <- optionMaybe restP
  spaces
  return (case rest of
    Nothing   -> t
    Just (":=", t') -> (case t of
      Var varName -> Assign varName t'
      _           -> error "Expected var")
    Just (op, t') -> Op (transOp op) t t')

restSeqP = do
  char ';'
  exprP

transOp s = case s of
  "+"  -> Plus
  "-"  -> Minus
  "*"  -> Times
  "/"  -> Divide
  ">=" -> Ge
  ">"  -> Gt
  "<=" -> Le
  "<"  -> Lt
  o    -> error $ "Unexpected operator " ++ o

-- Some string, followed by an expression
restP = do
  ch <- string "+"
    <|> string "-"
    <|> string "*"
    <|> string "/"
    <|> try (string "<=")
    <|> string "<"
    <|> try (string ">=")
    <|> string ">"
    <|> string ":=" -- not really a binary operator, but it fits in nicely here.
    <?> "binary operator"
  e <- exprP'
  return (ch, e)

-- All terms can be distinguished by looking at the first character
termP = valP
    <|> ifP
    <|> whileP
    <|> parenP
    <|> varP
    <?> "value, variable, 'if', 'while', or '('"


valP = do
  v <- boolP <|> numberP
  return $ Val v

boolP = do
  bStr <- string "true" <|> string "false" <|> string "skip"
  return $ case bStr of
    "true" -> BoolVal True
    "false" -> BoolVal False
    "skip" -> BoolVal False -- Treating the command 'skip' as a synonym for false, for ease of parsing

numberP = do
  n <- many1 digit
  return $ IntVal (read n) 

varP = do
  firstChar <- letter
  v <- many alphaNum
  return $ Var (firstChar:v)

ifP = do
  string "if"
  e1 <- exprP --' <|> valP
  string "then"
  e2 <- exprP --' <|> valP
  string "else"
  e3 <- exprP --' <|> valP
  string "endif"
  return $ If e1 e2 e3



whileP = do
  string "while"
  e1 <- exprP
  string "do"
  e2 <- exprP
  string "endwhile"
  return $ While e1 e2


-- An expression in parens, e.g. (9-5)*2
parenP = do
  string "("
  e <- exprP
  string ")"
  rest <- optionMaybe restP
  spaces
  return (case rest of
    Nothing -> e
    Just (op, e') -> Op (transOp op) e e')


-- This function will be useful for defining binary operations.
-- Unlike in the previous assignment, this function returns an "Either value".
-- The right side represents a successful computaton.
-- The left side is an error message indicating a problem with the program.
-- The first case is done for you.
applyOp :: Binop -> Value -> Value -> Either ErrorMsg Value
applyOp Plus (IntVal i) (IntVal j) = Right $ IntVal $ i + j
applyOp _ _ _ = error "TBD"


-- As with the applyOp method, the semantics for this function
-- should return Either values.  Left <error msg> indicates an error,
-- whereas Right <something> indicates a successful execution.
evaluate :: Expression -> Store -> Either ErrorMsg (Value, Store)
evaluate (Op o e1 e2) s = do
  (v1,s1) <- evaluate e1 s
  (v2,s') <- evaluate e2 s1
  v <- applyOp o v1 v2
  return (v, s')
evaluate _ _ = error "TBD"


-- Evaluates a program with an initially empty state
run :: Expression -> Either ErrorMsg (Value, Store)
run prog = evaluate prog Map.empty

showParsedExp fileName = do
  p <- parseFromFile fileP fileName
  case p of
    Left parseErr -> print parseErr
    Right exp -> print exp

runFile fileName = do
  p <- parseFromFile fileP fileName
  case p of
    Left parseErr -> print parseErr
    Right exp ->
      case (run exp) of
        Left msg -> print msg
        Right (v,s) -> print $ show s


