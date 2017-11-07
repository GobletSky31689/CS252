import Tokens

main = putStr "Hello Syntax"


-- | A compilation unit is the top level syntactic goal symbol of a Java program.
data CompilationUnit = CompilationUnit (Maybe PackageDecl) [ImportDecl] [TypeDecl]
  deriving (Eq,Show,Read)


 newtype PackageDecl = PackageDecl Name
  deriving (Eq,Show,Read)


 data ImportDecl
    = ImportDecl Name Bool -- Last Bool tells whether there is .* at the end or not


 data ArrayInit
    = ArrayInit [VarInit]
  deriving (Eq,Show,Read)