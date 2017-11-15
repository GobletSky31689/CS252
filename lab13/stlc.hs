import Data.Map (Map)
import qualified Data.Map as Map

-- We represent variables as strings.
type Variable = String

data Expression =
    ETrue
  | EFalse
  | EInt Int
  | ESucc Expression
  | EPred Expression
  | EIsZero Expression
  | EIf Expression Expression Expression
  | ELambda Variable StlcType Expression -- (\x:T.e)
  | EApp Expression Expression
  | EVar Variable
  deriving (Show)

data Value =
    VTrue
  | VFalse
  | VNum Int
  deriving (Show)

data StlcType =
    TBool
  | TInt
  | TFun StlcType StlcType  --  T1 -> T2
  deriving (Show,Eq)

type TypingEnv = Map Variable StlcType

--I'm using zero a lot, so I am making a shortcut
zero = EInt 0

typecheckFail e = error $ "Expression " ++ (show e) ++ " does not typecheck"

typecheck :: Expression -> TypingEnv -> StlcType
typecheck ETrue _ = TBool
typecheck EFalse _ = TBool
typecheck (EInt _) _ = TInt
typecheck e@(ESucc e1) env = case typecheck e1 env of
  TInt  -> TInt
  _     -> typecheckFail e
typecheck e@(EPred e1) env = case typecheck e1 env of
  TInt  -> TInt
  _     -> typecheckFail e
typecheck e@(EIf e1 e2 e3) env =
  let t1 = typecheck e1 env
      t2 = typecheck e2 env
      t3 = typecheck e3 env
  in if t1 == TBool && t2 == t3 then t2 else typecheckFail e
typecheck e@(EIsZero e1) env = case typecheck e1 env of
  TInt  -> TBool
  _     -> typecheckFail e
typecheck e@(EVar x) env = case (Map.lookup x env) of
  Just i -> i
  _      -> typecheckFail e
typecheck e@(ELambda x tin e') env = TFun tin tout
  where 
     tout = typecheck e' env'
     env' = Map.insert x tin env
typecheck e@(EApp e1 e2) env = case typecheck e1 env of
   TFun t1 t2 -> if t1 == typecheck e2 env then t2 else typecheckFail e
   _          -> typecheckFail e

--Some sample cases
test1 = typecheck (ESucc zero) Map.empty
test2 = typecheck (EPred (ESucc zero)) Map.empty
test3 = typecheck (EIf ETrue zero (ESucc (ESucc zero))) Map.empty
test4 = typecheck (ELambda "x" TInt ETrue) Map.empty
test5 = typecheck (EApp (ELambda "x" TInt (EIf (EIsZero (EVar "x")) (ESucc zero) zero)) (ESucc zero)) Map.empty

bad1 = typecheck (ESucc EFalse) Map.empty
bad2 = typecheck (EApp (ELambda "x" TInt (EIsZero (EVar "x"))) ETrue) Map.empty
