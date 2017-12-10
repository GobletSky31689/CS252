module Obfuscator where

import           Data.Char          (toLower)
import           Data.List          (intercalate)
import           Parser
import           Syntax
import           System.Environment


writeToFile fileName (CompilationUnit package imports classDecl) = do
    writeFile fileName (
            (getPackage package)
         ++ (getImports imports)
         ++ (getClassDecl classDecl)
         )


toLowerStr str = [toLower singleChar | singleChar <- str]


getClassDecl (ClassDecl modifiers name body) = (getModifiers modifiers) ++ "class " ++ (getName name)
                                                ++ "{\n" ++ (getClassBody body) ++ "\n}"


getMethod (MethodDecl modifiers ret_type name params statements) =
                                    (getModifiers modifiers)
                                    ++ (getType ret_type)
                                    ++ (getName name)
                                    ++ "(" ++ (getParams params) ++ ")"
                                    ++ "{\n" ++ (getStatement statements) ++ "\n}"


getLiteral literal = case literal of
    Int i -> show i
    Boolean bool -> show bool
    Real double -> show double


getBinOp x = case x of
    Plus -> "+"
    Minus -> "-"
    Times -> "*"
    Divide -> "/"
    Gt -> ">"
    Ge -> ">="
    Lt -> "<"
    Le -> "<="

getExpr exp = case exp of
    Lit literal -> getLiteral literal
    Var (VarAcc name) -> getName name
    Op binOp exp1 exp2 -> (getExpr exp1) ++ (getBinOp binOp) ++ (getExpr exp2)



getStatement statement = case statement of
    Sequence st1 st2 -> (getStatement st1) ++ "\n" ++ (getStatement st2)
    Declare (VarDecl _type name) exp -> (getType _type)
                               ++ (getName name)
                               ++ (case exp of
                                     Just exp' -> "=" ++ (getExpr exp')
                                     Nothing -> "")
                               ++ ";"
    Assign (VarAcc name) exp -> (getName name) ++ "=" ++ (getExpr exp) ++ ";"



getParams params = intercalate ", " [getParam param | param <- params]

getParam (FormalParameterDecl isFinal _type name) = (if isFinal then "final " else "")
                                    ++ (getType _type)
                                    ++ (getName name)


getType _type = case _type of
    PrimType VoidType                   -> "void "
    PrimType BooleanType                -> "bool "
    PrimType ByteType                   -> "byte "
    PrimType ShortType                  -> "short "
    PrimType IntType                    -> "int "
    PrimType LongType                   -> "long "
    PrimType CharType                   -> "char "
    PrimType FloatType                  -> "float "
    PrimType DoubleType                 -> "double "
    ClassRefType (Identifier className) -> className ++ " "




getClassBody (ClassBody methods) = intercalate "\n" [getMethod method | method <- methods]

getModifiers modifiers = (intercalate " " [getModifier modifier | modifier <- modifiers]) ++ " "

getModifier modifier = toLowerStr $ show modifier


getImports imports = (intercalate "\n" [(getImport _import) | _import <- imports]) ++ "\n"

getImport (ImportDecl isStatic name isWildCard) = "import " ++ (if isStatic then "static " else "")
                                                    ++ (getName name) ++ (if isWildCard then ".*;" else ";")

getPackage packageName = case packageName of
        Just (PackageDecl name) -> "package " ++ (getName name) ++ ";\n"

getName (Name identifiers) = intercalate "." [ x | (Identifier x) <- identifiers]





main = do
    args <- getArgs
    p <- getParsedExp $ head args
    case p of
        Left parseErr -> print parseErr
        Right ast     -> do
            writeFile ((head args) ++ "_obf.ast") (show ast)
            writeToFile ((head args) ++ "_obf.java") ast


