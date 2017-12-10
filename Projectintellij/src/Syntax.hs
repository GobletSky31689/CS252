module Syntax where

-- Syntax created from Chapter 18 of Java Specification book


-- First Bool is for static, second bool is for Wildcard(*) package imports
data ImportDecl
    = ImportDecl Bool Name Bool
    deriving (Eq,Show,Read)


data CompilationUnit = CompilationUnit (Maybe PackageDecl) [ImportDecl] ClassDecl
    deriving (Eq,Show,Read)


-- Only Implementing NormalClassDeclaration
-- No support for Enums
data ClassDecl = ClassDecl [Modifier] Name ClassBody
    deriving (Eq,Show,Read)


data Modifier
    = Public
    | Protected
    | Private
    | Static
    | Abstract
    | Final
    | Native
    | Synchronized
    | Transient
    | Volatile
    | Strictfp
    deriving (Eq,Show,Read)


data ClassBody = ClassBody [MethodDecl]
    deriving (Eq,Show,Read)


data MethodDecl = MethodDecl [Modifier] Type Name [FormalParameterDecl] Statement
    deriving (Eq,Show,Read)

-- Bool is for checking final or not
data FormalParameterDecl = FormalParameterDecl Bool Type Name
    deriving (Eq,Show,Read)


-- package com.gobletsky.obfuscator;
data PackageDecl = PackageDecl Name
    deriving (Eq,Show,Read)

data Statement
    = Declare VarDecl (Maybe Exp)
    | Assign VarAcc Exp
    | Sequence Statement Statement
    deriving (Eq,Show,Read)

-- Only supporting few expressions for now.
data Exp
    = Lit Literal
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
--     | ArrayType Type
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
    | VoidType
    deriving (Eq,Show,Read)


-- This is different from Primitive types above. Primitive define "type" of variable.
-- Literal are "fixed" values, that would be assigned to these variables.
-- TODO: Add String literal support
data Literal
    = Int Integer
    | Boolean Bool
    | Real Double
    deriving (Eq,Show,Read)


-- same as HW3
data BinOp
    = Plus
    | Minus
    | Times
    | Divide
    | Gt
    | Ge
    | Lt
    | Le
    deriving (Eq,Show,Read)
