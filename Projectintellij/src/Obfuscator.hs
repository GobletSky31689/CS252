module Obfuscator where

import Data.List (intercalate)
import Syntax
import Parser
import System.Environment


writeToFile fileName (CompilationUnit package imports classDecl) = do
    writeFile fileName (getPackage package)





getPackage packageName = case packageName of
--         Left parseErr -> ""
        Just (PackageDecl name) -> "package " ++ (getName name) ++ ";"

getName (Name identifiers) = intercalate "." [ x | (Identifier x) <- identifiers]





main = do
    args <- getArgs
    p <- getParsedExp $ head args
    case p of
        Left parseErr -> print parseErr
        Right ast -> writeToFile ((head args) ++ "_obf.java") ast


