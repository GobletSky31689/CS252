module Obfuscator where

import           Data.Char          (toLower, isAlpha, isNumber, isUpper, toUpper)
import           Data.List          (intercalate, foldl')
import qualified Data.Map           as Map
import           Parser
import           Syntax
import           System.Environment (getArgs)
import System.Random (newStdGen, randomRs)
import System.IO.Unsafe


writeToFile :: FilePath -> CompilationUnit -> IO ()
writeToFile fileName (CompilationUnit package imports classDecl) = writeFile fileName (
            (getPackage package)
         ++ (getImports imports)
         ++ (getClassDecl classDecl Map.empty)
         )


toLowerStr :: [Char] -> [Char]
toLowerStr str = [toLower singleChar | singleChar <- str]


getClassDecl :: ClassDecl -> Map.Map [Char] [Char] -> [Char]
getClassDecl (ClassDecl modifiers name body) mapping = (getModifiers modifiers) ++ "class obf_" ++ (getName name)
                                                ++ "{\n" ++ (getClassBody body mapping) ++ "\n}"


getMethod :: MethodDecl -> Map.Map [Char] [Char] -> ([Char],  Map.Map [Char] [Char])
getMethod (MethodDecl modifiers ret_type name params statements) mapping =
                                    ((getModifiers modifiers)
                                    ++ (getType ret_type)
                                    ++ (obf_name)
                                    ++ "(" ++ formal_params ++ ")"
                                    ++ "{\n" ++ obf_statements ++ "\n}", mapping''')
                                    where (obf_name, mapping') = getObfName name mapping
                                          (formal_params, mapping'') = (getParams params mapping')
                                          (obf_statements, mapping''') = getAltStatement statements mapping''




getLiteral :: Literal -> String
getLiteral literal = case literal of
    Int i        -> show i
    Boolean bool -> show bool
    Real double  -> show double


getBinOp :: BinOp -> [Char]
getBinOp x = case x of
    Plus   -> "+"
    Minus  -> "-"
    Times  -> "*"
    Divide -> "/"
    Gt     -> ">"
    Ge     -> ">="
    Lt     -> "<"
    Le     -> "<="


getObfName :: Name -> Map.Map [Char] [Char] -> ([Char], Map.Map [Char] [Char])
getObfName name mapping = case (Map.lookup orig_name mapping) of
                                 Just s' -> (s', mapping)
                                 Nothing -> if ('.' `elem` orig_name) then (orig_name, mapping) else (s, mapping')
                                    where mapping' = storeObfName name mapping
                                          (s, _) = getObfName name mapping'
                          where orig_name = getName name


storeObfName :: Name -> Map.Map [Char] [Char] -> Map.Map [Char] [Char]
storeObfName name mapping = Map.insert orig_name obf_name mapping
                          where orig_name = getName name
                                obf_name = take 10 $ randomRs ('a','z') $ unsafePerformIO newStdGen


getExpr :: Exp -> Map.Map [Char] [Char] -> [Char]
getExpr exp mapping = case exp of
    Lit literal -> getLiteral literal
    Var (VarAcc name) -> obf_name
        where (obf_name, _) = getObfName name mapping
    Op binOp exp1 exp2 -> (getExpr exp1 mapping) ++ (getBinOp binOp) ++ (getExpr exp2 mapping)


getAltStatement :: Statement -> Map.Map [Char] [Char] -> ([Char], Map.Map [Char] [Char])
getAltStatement statement mapping = case statement of
    Sequence st1 st2 -> (firstStatement ++ "\n" ++ secondStatement, mapping'')
                    where (firstStatement, mapping') = (getAltStatement st1 mapping)
                          (secondStatement, mapping'') = (getAltStatement st2 mapping')

    Declare (VarDecl _type name) exp -> (((getType _type)
                                   ++ (obf_name)
                                   ++ (case exp of
                                         Just exp' -> "=" ++ (getExpr exp' mapping')
                                         Nothing -> "")
                                   ++ ";"), mapping')
                                   where (obf_name, mapping') = getObfName name mapping

    Assign (VarAcc name) exp -> ((obf_name ++ "=" ++ (getExpr exp mapping) ++ ";"), mapping)
                        where (obf_name, _) = getObfName name mapping
    Return exp -> ("return " ++ (getExpr exp mapping) ++ ";", mapping)
    MethodCall name args -> (obf_name ++ "(" ++ argsList ++ ");", mapping'')
                            where (obf_name, mapping') = getObfName name mapping
                                  (argsList, mapping'') = getMethodArgs args mapping'



getMethodArgs :: [ArgumentDecl] -> Map.Map [Char] [Char] -> ([Char], Map.Map [Char] [Char])
getMethodArgs args mapping = (intercalate ", " [item | (item, _) <- new_list], mapping)
                    where new_list = [getObfName name mapping | (ArgumentDecl name) <- args]


getParams :: Foldable t => t FormalParameterDecl -> Map.Map [Char] [Char] -> ([Char], Map.Map [Char] [Char])
getParams params mapping = (intercalate ", " reversedList, new_mapping)
    where   (newParamList, new_mapping) = foldl storeParams ([], mapping) params
            reversedList = reverse newParamList -- IIWIW:)


storeParams :: ([[Char]], Map.Map [Char] [Char]) -> FormalParameterDecl -> ([[Char]], Map.Map [Char] [Char])
storeParams (acc_args, acc_mapping) param = ((param' : acc_args), new_acc_mapping)
    where (param', new_acc_mapping) = getParam param acc_mapping


getParam :: FormalParameterDecl -> Map.Map [Char] [Char] -> ([Char], Map.Map [Char] [Char])
getParam (FormalParameterDecl isFinal _type name) mapping = ((if isFinal then "final " else "")
                                    ++ (getType _type)
                                    ++ obf_name, mapping')
                                    where (obf_name, mapping') = getObfName name mapping


getType :: Type -> [Char]
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


getClassBody :: ClassBody -> Map.Map [Char] [Char] -> [Char]
getClassBody (ClassBody methods) mapping = intercalate "\n" reversedList
    where  (newMethodList, new_mapping) = foldl storeMethods ([], mapping) methods
           reversedList = reverse newMethodList -- IIWIW:)

storeMethods (acc_methods, acc_mapping) method = ((method' : acc_methods), new_acc_mapping)
    where (method', new_acc_mapping) = getMethod method acc_mapping


getModifiers :: Show a => [a] -> [Char]
getModifiers modifiers = (intercalate " " [getModifier modifier | modifier <- modifiers]) ++ " "


getModifier :: Show a => a -> [Char]
getModifier modifier = toLowerStr $ show modifier


getImports :: [ImportDecl] -> [Char]
getImports imports = (intercalate "\n" [(getImport _import) | _import <- imports]) ++ "\n"


getImport :: ImportDecl -> [Char]
getImport (ImportDecl isStatic name isWildCard) = "import " ++ (if isStatic then "static " else "")
                                                    ++ (getName name) ++ (if isWildCard then ".*;" else ";")

getPackage :: Maybe PackageDecl -> [Char]
getPackage packageName = case packageName of
        Just (PackageDecl name) -> "package " ++ (getName name) ++ ";\n"


getName :: Name -> [Char]
getName (Name identifiers) = intercalate "." [ x | (Identifier x) <- identifiers]


main :: IO ()
main = do
    args <- getArgs
    p <- getParsedExp $ head args
    case p of
        Left parseErr -> print parseErr
        Right ast     -> do
            writeFile ((head args) ++ "_obf.ast") (show ast)
            writeToFile ("obf_" ++ (head args)) ast


