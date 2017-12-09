import Text.ParserCombinators.Parsec
import System.Environment
import Syntax


singleString :: GenParser Char st String
singleString = do
    x <- many1 letter
    return x


modifier :: GenParser Char st Modifier
modifier = do
    ch <- string "public"
        <|> string "private"
        <?> "modifier"
    return $ transModifier ch



classDeclP :: GenParser Char st ClassDecl
classDeclP = do
    x <- many modifier
    spaces
    string "class"
    spaces
    name <- singleString
    spaces
    char '{'
    spaces
    body <- classBodyP
    spaces
    char '}'
    return $ ClassDecl [] (Name [Identifier name]) body



classBodyP :: GenParser Char st ClassBody
classBodyP = do
    string "public static void main (String[] args) {"
    spaces
    statments <- statementP
    spaces
    char '}'
    spaces
    return $ ClassBody statments





importP :: GenParser Char st ImportDecl
importP = do
    string "import"
    spaces
    isStatic <- optionMaybe $ string "static"
    spaces
    package <- singleString `sepBy` (char '.')
  -- spaces
  -- isUniversal <- optionMaybe $ string ".*"
    spaces
    char ';'
    spaces
    return $ ImportDecl True (Name [Identifier x1 | x1 <- package]) True


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
-- Java Program like PackageDecl -> ImportStmts -> ClassDecls
-- ClassDecls can further have FieldDecls and MethodDecls
-- MethoodDecls can further have sequence of Exp
-- E.g. somethin like this:
-- data JavaPrg = (Maybe PackageDecl) [ImportStmt] [ClassDecl]
-- data ClassDecl = [VarDecl] [Methodecl] -- We can change the order of var & method decl as it should not matter
-- data Methodecl = [Statement]
fileP :: GenParser Char st CompilationUnit
fileP = do
  -- TODO: use it when heirarchy is defines
    pkgName <- optionMaybe packageNameP
    imports <- many importP
    classDecls <- classDeclP
    eof
    return $ CompilationUnit pkgName imports classDecls


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


-- Utility function to transform string to Modifier
transModifier s = case s of
    "public"  -> Public
    "private"  -> Private



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
            return $ Assign (translVar var) expr
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
        <?> "binary operator"
    e <- exprP
    return (ch, e)


varP :: GenParser Char st String
varP = do
    spaces
    firstChar <- letter
    v <- many alphaNum
    let s = firstChar:v
    return s

-- Convert string obtained from varP to a Exp
transrVar :: Monad m => m String -> m Exp
transrVar s = do
    str <- s
    return $ Var (VarAcc (Name [Identifier str]))

translVar :: String -> VarAcc
translVar s = VarAcc (Name [Identifier s])


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


main = do
    args <- getArgs
    p <- parseFromFile fileP $ head args
    case p of
        Left parseErr -> print parseErr
        Right ast -> writeFile ((head args) ++ "_obf.ast") (show ast)

