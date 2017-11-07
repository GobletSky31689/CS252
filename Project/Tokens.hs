module Tokens where

data Type
    = PrimType Primitives
    | RefType References
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

data References
    = ClassRefType Identifier
    | ArrayType Type
  deriving (Eq,Show,Read)

--  A single identifier.
data Identifier = Identifier String
    deriving (Eq,Show,Read)

--  A package Name.
data Name = Name [Identifier]
    deriving (Eq,Show,Read)

data CompilationUnit = CompilationUnit (Maybe PackageDecl) [ImportDecl] [ClassDecl]
  deriving (Eq,Show,Read)

-- public static class MyClass
data ClassDecl
    = ClassDecl [Modifier] Identifier ClassBody
  deriving (Eq,Show,Read)

data VarDeclId
    = VarId Identifier
    | VarDeclArray VarDeclId
    -- ^ Multi-dimensional arrays are represented by nested applications of 'VarDeclArray'.
  deriving (Eq,Show,Read)

data ArrayInit
    = ArrayInit [VarInit]
  deriving (Eq,Show,Read)

-- | A declaration of a variable, which may be explicitly initialized.
data VarDecl
    = VarDecl VarDeclId (Maybe VarInit)
  deriving (Eq,Show,Read)

data VarInit
    = InitExp Exp
    | InitArray ArrayInit
  deriving (Eq,Show,Read)

data Decl
    = FieldDecl  [Modifier] Type [VarDecl]
    | MethodDecl [Modifier] Type Identifier [Arguments] MethodBody
  deriving (Eq,Show,Read)

newtype MethodBody = MethodBody (Maybe Block)
  deriving (Eq,Show,Read)

data Block = Block [BlockStmt]
  deriving (Eq,Show,Read)

data BlockStmt
    = BlockStmt Stmt
    | LocalClass ClassDecl
    | LocalVars [Modifier] Type [VarDecl]
  deriving (Eq,Show,Read)

data Arguments = Arguments [Modifier] Type VarDeclId
  deriving (Eq,Show,Read)

newtype ClassBody = ClassBody [Decl]
  deriving (Eq,Show,Read)

newtype PackageDecl = PackageDecl Name
  deriving (Eq,Show,Read)

data ImportDecl
    = ImportDecl Bool {- static? -} Name Bool {- .*? -}
  deriving (Eq,Show,Read)

data Modifier
    = Public
    | Private
    | Protected
    | Abstract
    | Final
    | Static
  deriving (Eq,Show,Read)


-- data Stmt
--     -- | A statement can be a nested block.
--     = StmtBlock Block
--     -- | The @if-then@ statement allows conditional execution of a statement.
--     | IfThen Exp Stmt
--     -- | The @if-then-else@ statement allows conditional choice of two statements, executing one or the other but not both.
--     | IfThenElse Exp Stmt Stmt
--     -- | The @while@ statement executes an expression and a statement repeatedly until the value of the expression is false.
--     | While Exp Stmt
--     -- | The basic @for@ statement executes some initialization code, then executes an expression, a statement, and some
--     --   update code repeatedly until the value of the expression is false.
--     | BasicFor (Maybe ForInit) (Maybe Exp) (Maybe [Exp]) Stmt
--     -- | The enhanced @for@ statement iterates over an array or a value of a class that implements the @iterator@ interface.
--     | EnhancedFor [Modifier] Type Ident Exp Stmt
--     -- | An empty statement does nothing.
--     | Empty
--     -- | Certain kinds of expressions may be used as statements by following them with semicolons:
--     --   assignments, pre- or post-inc- or decrementation, method invocation or class instance
--     --   creation expressions.
--     | ExpStmt Exp
--     -- | An assertion is a statement containing a boolean expression, where an error is reported if the expression
--     --   evaluates to false.
--     | Assert Exp (Maybe Exp)
--     -- | The switch statement transfers control to one of several statements depending on the value of an expression.
--     | Switch Exp [SwitchBlock]
--     -- | The @do@ statement executes a statement and an expression repeatedly until the value of the expression is false.
--     | Do Stmt Exp
--     -- | A @break@ statement transfers control out of an enclosing statement.
--     | Break (Maybe Ident)
--     -- | A @continue@ statement may occur only in a while, do, or for statement. Control passes to the loop-continuation
--     --   point of that statement.
--     | Continue (Maybe Ident)
--     -- A @return@ statement returns control to the invoker of a method or constructor.
--     | Return (Maybe Exp)
--     -- | A @synchronized@ statement acquires a mutual-exclusion lock on behalf of the executing thread, executes a block,
--     --   then releases the lock. While the executing thread owns the lock, no other thread may acquire the lock.
--     | Synchronized Exp Block
--     -- | A @throw@ statement causes an exception to be thrown.
--     | Throw Exp
--     -- | A try statement executes a block. If a value is thrown and the try statement has one or more catch clauses that
--     --   can catch it, then control will be transferred to the first such catch clause. If the try statement has a finally
--     --   clause, then another block of code is executed, no matter whether the try block completes normally or abruptly,
--     --   and no matter whether a catch clause is first given control.
--     | Try Block [Catch] (Maybe {- finally -} Block)
--     -- | Statements may have label prefixes.
--     | Labeled Ident Stmt
--   deriving (Eq,Show,Read)





