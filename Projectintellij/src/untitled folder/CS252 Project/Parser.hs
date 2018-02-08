module Parser where

import Text.ParserCombinators.Parsec
import System.Environment
import Syntax
import Data.Maybe (isJust)
import Data.String.Utils (startswith, endswith)


singleString :: GenParser Char st String
singleString = do
    x <- many1 (oneOf "[]" <|> letter)
    return x



-- singleStringForType :: GenParser Char st String
-- singleStringForType = do
--     x <- many1 (oneOf "[]" <|> letter)
--     return x


types :: GenParser Char st Type
types = do
    ch <- singleString
--     if (endswith "[]" ch)
--     then
--         return $ (ArrayType (transType ch))
--     else
    return $ transType ch

modifier :: GenParser Char st Modifier
modifier = do
    ch <- string "static"
        <|> string "public"
        <|> string "private"
        <?> "modifier"
    spaces
    return $ transModifier ch



classDeclP :: GenParser Char st ClassDecl
classDeclP = do
    x <- many1 modifier
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
    return $ ClassDecl x (Name [Identifier name]) body



classBodyP :: GenParser Char st ClassBody
classBodyP = do
    spaces
    methods <- many methodP
    spaces
    return $ ClassBody methods



formalParamP :: GenParser Char st FormalParameterDecl
formalParamP = do
    spaces
    final <- optionMaybe $ string "final"
    spaces
    arg_type <- types
    spaces
    name <- singleString
    spaces
    return $ FormalParameterDecl (isJust final) arg_type (Name [Identifier name])



methodP :: GenParser Char st MethodDecl
methodP = do
    spaces
    x <- many modifier
    spaces
    ret_type <- types
    spaces
    name <- singleString
    spaces
    char '('
    spaces
    params <- formalParamP `sepBy` (char ',')
    spaces
    char ')'
    spaces
    char '{'
    statements <- statementP
    -- Note that statementP will eat the closing braces!!
    spaces
--     if ((startswith "[]" name) || (endswith "[]" name))
--     then
--         return $ MethodDecl x (ArrayType ret_type) (Name [Identifier name]) params statements
--     else
    return $ MethodDecl x ret_type (Name [Identifier name]) params statements


importP :: GenParser Char st ImportDecl
importP = do
    string "import"
    spaces
    static <- optionMaybe $ string "static"
    spaces
    package <- (many (noneOf (". \n;*"))) `sepBy` (char '.')
    spaces
    wildCardImport <- optionMaybe $ string "*"
    spaces
    char ';'
    spaces
    if (isJust wildCardImport)
        then return $ ImportDecl (isJust static) (Name [Identifier x1 | x1 <- (init package)]) True
        else return $ ImportDecl (isJust static) (Name [Identifier x1 | x1 <- package]) False


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
-- MethodDecls can further have sequence of Exp
-- E.g. something like this:
-- data JavaPrg = (Maybe PackageDecl) [ImportStmt] [ClassDecl]
-- data ClassDecl = [VarDecl] [MethodDecl] -- We can change the order of var & method decl as it should not matter
-- data MethodDecl = [Statement]
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
    "static"  -> Static



-- Utility function to transform string to Type
transType s = case s of
    "void"  -> PrimType VoidType
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
    char ';'
    spaces
    blockFinish <- optionMaybe (char '}')
    case blockFinish of
        Nothing -> do
            e' <- statementP
            return $ Sequence e e'
        Just _ -> return e


-- restSeqP :: GenParser Char st Statement
-- restSeqP = do
--     char ';'
--     statementP

-- Parse a single statement
statementP' :: GenParser Char st Statement
statementP' = do
    spaces
    -- Only Variable Declaration & Assignment statements supported for now
    -- TODO: Changing the order of assign & Decl causes errors. Can it be fixed??
    stmnt <- returnP <|> varAssignDeclP
    spaces
    return stmnt



returnP :: GenParser Char st Statement
returnP = do
    string "return"
    spaces
    expr <- exprP
    spaces
    return $ Return expr


getMethodArgsP :: GenParser Char st [ArgumentDecl]
getMethodArgsP = do
    vars <- varP `sepBy` (char ',')
    spaces
    char ')'
    spaces
    return $ [ArgumentDecl (Name [Identifier x]) | x <- vars]


varAssignDeclP :: GenParser Char st Statement
varAssignDeclP = do
    var <- varP `sepBy` (char '.')
    spaces
    isAssignStmt <- optionMaybe (char '=')
    spaces
    case isAssignStmt of
        Just _ -> do
            expr <- exprP
            return $ Assign (translVar (head var)) expr
        Nothing -> do
            isMethodCall <- optionMaybe (char '(')
            case isMethodCall of
                Just _ -> do
                    args <- getMethodArgsP
                    spaces
                    return $ MethodCall $ MethodExp (getNameFromArr var) args
                Nothing -> do
                    var' <- varP `sepBy` (char '.')
                    spaces
                    isAssignStmtAlso <- optionMaybe (char '=')
                    spaces
                    case isAssignStmtAlso of
                        Just y -> do
                            expr <- optionMaybe exprP
                            return $ Declare (VarDecl (transType (head var)) (getNameFromArr var')) expr
                        Nothing -> return $ Declare (VarDecl (transType (head var)) (getNameFromArr var')) Nothing


getNameFromArr x = (Name [Identifier x1 | x1 <- x])


getNameFromVar (Var (VarAcc x)) = x

exprP :: GenParser Char st Exp
exprP = do
    spaces
    lExpr <- (transrVar varP) <|> valP
    spaces
    rest <- optionMaybe restP
    case rest of
        Nothing   -> do
            isMethodCall <- optionMaybe (char '(')
            case isMethodCall of
                Just _ -> do
                    args <- getMethodArgsP
                    spaces
                    return $ MethodExp (getNameFromVar lExpr) args
                Nothing -> return lExpr
        Just (op, rExpr) -> return $ Op (transOp op) lExpr rExpr


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


getParsedExp fileName = do
    p <- parseFromFile fileP fileName
    return p


showParsedExp fileName = do
    p <- parseFromFile fileP fileName
    case p of
        Left parseErr -> print parseErr
        Right exp -> print exp


