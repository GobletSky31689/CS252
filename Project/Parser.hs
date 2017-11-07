import Text.ParserCombinators.Parsec

-- package com.gobletsky.obfuscator;
data PackageDecl = PackageDecl Name
  	deriving (Eq,Show,Read)

data Statement = Declare VarDecl (Maybe Exp)
				| Assign VarAcc Exp
		    	| Sequence Statement Statement 
	deriving (Eq,Show,Read)

-- Only supporting few expressions for now.
data Exp = Lit Literal
	    	| Var VarAcc
	    	| Op BinOp Exp Exp
	    	deriving (Eq,Show,Read)

-- An variable declaration
data VarDecl = VarDecl Type Name
			deriving (Eq,Show,Read)

data VarAcc = VarAcc Name
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
-- TODO: Add String literal support
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
	spaces
	char ';'
	spaces
	return $ PackageDecl (Name [Identifier x1 | x1 <- x])


-- TODO: Create a ADT that simulates the heirarchy of a typical
-- Java Program like PackageDel -> ImportStmts -> ClassDecls
-- ClassDecls can further have FieldDecls and MethodDecls
-- MethoodDecls can further have sequence of Exp
fileP :: GenParser Char st Statement
fileP = do
  -- TODO: use it when heirarchy is defines
  pkgName <- optionMaybe packageNameP
  prog <- statementP
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
statementP :: GenParser Char st Statement
statementP = do
  e <- statementP'
  rest <- optionMaybe restSeqP
  return (case rest of
    Nothing -> e
    Just e' -> Sequence e e')

restSeqP :: GenParser Char st Statement
restSeqP = do
  char ';'
  statementP

-- Parse a single statement
statementP' :: GenParser Char st Statement
statementP' = do
  spaces
  -- Only Variable Declaration & Assignment statements supported for now
  -- TODO: Changing the order of assign & Decl causes errors. Can it be fixed??
  stmnt <- varAssignDeclP
  spaces
  return stmnt


varAssignDeclP :: GenParser Char st Statement
varAssignDeclP = do
  var <- varP
  spaces
  isAssignStmt <- optionMaybe (char '=')
  spaces
  case isAssignStmt of 
   			Just x -> do
   				expr <- exprP
   				return $ Assign (VarAcc (Name [Identifier var])) expr
   			Nothing -> do
  	        	var' <- varP
  	        	return $ Declare (VarDecl (transType var) (Name [Identifier var'])) Nothing
  

exprP :: GenParser Char st Exp
exprP = do
	spaces
	lExpr <- (transrVar varP) <|> valP
	spaces
	rest <- optionMaybe restP
	return (case rest of
	    Nothing   -> lExpr
	    Just (op, rExpr) -> Op (transOp op) lExpr rExpr)


restP = do
  ch <- string "+"
    <|> string "-"
    <|> string "*"
    <|> string "/"
    <|> try (string "<=")
    <|> string "<"
    <|> try (string ">=")
    <|> string ">"
    -- <|> string "=" -- not really a binary operator, but it fits in nicely here.
    <?> "binary operator"
  e <- exprP
  return (ch, e)

varP = do
  spaces
  firstChar <- letter
  v <- many alphaNum
  let s = firstChar:v
  return s


transrVar s = do
	str <- s
	return $ Var (VarAcc (Name [Identifier str]))

-- translVar s = do
-- 	str <- s
-- 	return $ (VarAcc (Name [Identifier str]))





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


showParsedExp fileName = do
  p <- parseFromFile fileP fileName
  case p of
    Left parseErr -> print parseErr
    Right exp -> print exp


