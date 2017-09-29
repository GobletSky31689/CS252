{-
  Name: Ujjawal Garg
  Class: CS 252
  Assigment: HW2
  Date: 28 Sep 2017
  Description: An interpreter with integers, booleans and while loops supported
-}


module WhileInterp (
  Expression(..),
  Binop(..),
  Value(..),
  testProgram,
  run
) where


import Data.Map (Map)
import qualified Data.Map as Map

-- We represent variables as strings.
type Variable = String

-- The store is an associative map from variables to values.
-- (The store roughly corresponds with the heap in a language like Java).
type Store = Map Variable Value

data Expression =
    Var Variable                            -- x
  | Val Value                               -- v
  | Assign Variable Expression              -- x := e
  | Sequence Expression Expression          -- e1; e2
  | Op Binop Expression Expression
  | If Expression Expression Expression     -- if e1 then e2 else e3
  | While Expression Expression             -- while (e1) e2
  | AND Expression Expression               -- e1 AND e2
  | OR Expression Expression               -- e1 OR e2
  | NOT Expression                          -- NOT e1
  deriving (Show)

data Binop =
    Plus     -- +  :: Int  -> Int  -> Int
  | Minus    -- -  :: Int  -> Int  -> Int
  | Times    -- *  :: Int  -> Int  -> Int
  | Divide   -- /  :: Int  -> Int  -> Int
  | Gt       -- >  :: Int -> Int -> Bool
  | Ge       -- >= :: Int -> Int -> Bool
  | Lt       -- <  :: Int -> Int -> Bool
  | Le       -- <= :: Int -> Int -> Bool
  deriving (Show)

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show)


-- This function will be useful for defining binary operations.
-- The first case is done for you.
-- Be sure to explicitly check for a divide by 0 and throw an error.
applyOp :: Binop -> Value -> Value -> Value
applyOp Plus (IntVal i) (IntVal j) = IntVal $ i + j
applyOp Minus (IntVal i) (IntVal j) = IntVal $ i - j
applyOp Times (IntVal i) (IntVal j) = IntVal $ i * j
applyOp Divide (IntVal i) (IntVal 0) = error "Division by 0 not supported"
applyOp Divide (IntVal i) (IntVal j) = IntVal $ i `div` j
applyOp Gt (IntVal i) (IntVal j) = BoolVal $ i > j
applyOp Ge (IntVal i) (IntVal j) = BoolVal $ i >= j
applyOp Lt (IntVal i) (IntVal j) = BoolVal $ i < j
applyOp Le (IntVal i) (IntVal j) = BoolVal $ i <= j
applyOp _ _ _ = error "Binary operations not supported for Boolean Values"


-- Implement this function according to the specified semantics
evaluate :: Expression -> Store -> (Value, Store)
evaluate (Val val) s = (val, s)

-- [ss-access-red]
evaluate (Var var) s = case (Map.lookup var s) of
    Just i -> (i,s)
    _      -> error ("Key \"" ++ var ++ "\" is not in the map")

-- [ss-seq-red]
evaluate (Sequence (Val v) e2) s = evaluate e2 s
-- [ss-seq-context]
evaluate (Sequence e1 e2) s = evaluate (Sequence (Val e1') e2) s'
  where (e1', s') = evaluate e1 s

-- [ss-assign-red]
evaluate (Assign x (Val v)) s = (v, (Map.insert x v s))
-- [ss-assign-context]
evaluate (Assign x e) s = evaluate (Assign x (Val e')) s'
  where (e', s') = evaluate e s

-- [ss-iftrue-red]
evaluate (If (Val (BoolVal True)) e1 e2) s = evaluate e1 s
-- [ss-iffalse-red]
evaluate (If (Val (BoolVal False)) e1 e2) s = evaluate e2 s
-- [ss-if-context]
evaluate (If e1 e2 e3) s = evaluate (If (Val e1') e2 e3) s'
  where (e1', s') = evaluate e1 s

-- [ss-op-red]
evaluate (Op o (Val v1) (Val v2)) s = (applyOp o v1 v2, s)
-- [ss-op-context-1]
evaluate (Op o (Val v1) e) s = evaluate (Op o (Val v1) (Val e')) s'
  where (e', s') = evaluate e s
-- [ss-op-context-2]
evaluate (Op o e1 e2) s = evaluate (Op o (Val e1') e2) s'
  where (e1', s') = evaluate e1 s


-- [ss-and-red-1]
evaluate (AND (Val (BoolVal True)) e2) s = evaluate e2 s
-- [ss-and-red-2]
evaluate (AND (Val (BoolVal False)) e2) s = (BoolVal False, s)
-- [ss-and-context]
evaluate (AND e1 e2) s = evaluate (AND (Val e1') e2) s' 
  where (e1', s') = evaluate e1 s


-- [ss-or-red-1]
evaluate (OR (Val (BoolVal False)) e2) s = evaluate e2 s
-- [ss-or-red-2]
evaluate (OR (Val (BoolVal True)) e2) s = (BoolVal True, s)
-- [ss-or-context]
evaluate (OR e1 e2) s = evaluate (OR (Val e1') e2) s' 
  where (e1', s') = evaluate e1 s


-- [ss-not-red-1]
evaluate (NOT (Val (BoolVal True))) s = (BoolVal False, s)
-- [ss-not-red-2]
evaluate (NOT (Val (BoolVal False))) s = (BoolVal True, s)
-- [ss-not-context]
evaluate (NOT e) s = evaluate (NOT (Val e')) s' 
  where (e', s') = evaluate e s

-- [ss-while]
evaluate (While e1 e2) s = evaluate (If e1 (Sequence e2 (While e1 e2)) (Val (BoolVal False))) s

-- evaluate _ _ = error "TBD"


-- Evaluates a program with an initially empty state
run :: Expression -> (Value, Store)
run prog = evaluate prog Map.empty

-- The same as run, but only returns the Store
testProgram :: Expression -> Store
testProgram prog = snd $ run prog


