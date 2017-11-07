import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import System.Environment

-- package com.gobletsky.obfuscator;
data PackageDecl = PackageDecl Name
  deriving (Eq,Show,Read)

-- Only supporting few expressions for now.
data Exp = Lit Literal
	    	| Var Variable
	    	| Assign Variable Exp
	    	| Sequence Exp Exp 
	    	| Op BinOp Exp Exp
	    	deriving (Eq,Show,Read)

-- Any variable object
data Variable = Variable Type Name
			deriving (Eq,Show,Read)

--  A single identifier. Could be a variable, className etc
data Identifier = Identifier String
    deriving (Eq,Show,Read)

-- '.' separated string. It could be a package, field, method, class
-- e.g. java.lang.System package or BigInteger.ONE field etc
-- Will mostly use Name instead of Identifier as 
-- it is more broader
data Name = Name [Identifier]
    deriving (Eq,Show,Read)

-- TODO: Add support for Array Types
-- There can be two types of variables: Primitive, ClassReference
data Type
    = PrimType Primitives
    | ClassRefType Identifier
  deriving (Eq,Show,Read)

data Primitives
    = BooleanType
    | ByteType
    | ShortType
    | IntType
    | LongType
    | CharType
    | FloatType
    | DoubleType
  deriving (Eq,Show,Read)

-- This is different from Primitive types above. Primitive define "type" of variable.
-- Literal are "fixed" values, that would be assigned to these variables.
data Literal = Int Integer
		    | Boolean Bool
		    | Real Double
  			deriving (Eq,Show,Read)

-- same as HW3
data BinOp = Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  deriving (Eq,Show,Read)


singleString :: GenParser Char st String
singleString = do
	x <- many1 letter
	return x

packageNameP :: GenParser Char st PackageDecl
packageNameP = do
	string "package"
	spaces
	x <- singleString `sepBy` (char '.')
	char ';'
	spaces
	return $ PackageDecl (Name [Identifier x1 | x1 <- x])


-- TODO: Create a ADT that simulates the heirarchy of a typical
-- Java Program like PackageDel -> ImportStmts -> ClassDecls
-- ClassDecls can further have FieldDecls and MethodDecls
-- MethoodDecls can further have sequence of Exp
fileP :: GenParser Char st Exp
fileP = do
  -- TODO: use it when heirarchy is defines
  pkgName <- optionMaybe packageNameP
  prog <- exprP
  eof
  return prog


-- Utility function to transform string to BinOp
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

-- Utility function to transform string to Type
transType s = case s of
  "bool"  -> PrimType BooleanType
  "byte"  -> PrimType ByteType
  "short"  -> PrimType ShortType
  "int"  -> PrimType IntType
  "long" -> PrimType LongType
  "char"  -> PrimType CharType
  "double"  -> PrimType DoubleType
  o    -> ClassRefType (Identifier o)

-- TODO: create transforms functions for other keywords


-- Parse a sequence of statements
exprP = do
  e <- exprP'
  rest <- optionMaybe restSeqP
  return (case rest of
    Nothing -> e
    Just e' -> Sequence e e')

-- Parse a single statement
exprP' = do
  spaces
  t <- termP
  spaces
  rest <- optionMaybe restP
  spaces
  return (case rest of
    Nothing   -> t
    Just ("=", t') -> (case t of
      Var varName -> Assign varName t'
      _           -> error "Expected var")
    Just (op, t') -> Op (transOp op) t t')


restP = do
  ch <- string "+"
    <|> string "-"
    <|> string "*"
    <|> string "/"
    <|> try (string "<=")
    <|> string "<"
    <|> try (string ">=")
    <|> string ">"
    <|> string "=" -- not really a binary operator, but it fits in nicely here.
    <?> "binary operator"
  e <- exprP'
  return (ch, e)



restSeqP = do
  char ';'
  exprP

termP = valP
    <|> varP
    <?> "value, variable"

valP = do
  v <- boolP <|> numberP
  return $ Lit v


boolP = do
  bStr <- string "True" <|> string "False"
  return $ case bStr of
    "True" -> Boolean True
    "False" -> Boolean False

numberP = do
  n <- many1 digit
  isFloat <- optionMaybe (char '.')
  case isFloat of
  	Nothing -> return $ Int (read n)
  	Just x -> do
  		f <- many1 digit
  		return $ Real (read (n ++ "." ++ f))

varP = do
  typeStr <- many1 alphaNum
  spaces
  firstChar <- letter
  v <- many alphaNum
  let s = firstChar:v
  return $ Var (Variable (transType typeStr) (Name [Identifier s]))

showParsedExp fileName = do
  p <- parseFromFile fileP fileName
  case p of
    Left parseErr -> print parseErr
    Right exp -> print exp


