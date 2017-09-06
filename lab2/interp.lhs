This File is _literate Haskell_.
That means that (in some sense) code and comments are reversed.
By default, everything that I type is actually a comment.

To write code, I preface it with a 'greater than' symbol.
Here we define the expressions in our language:

> data Exp = ETrue
>          | EFalse
>          | Eif Exp Exp Exp
>		   | EInt Int
>		   | EPred Exp
>		   | ESucc Exp
>   deriving Show

When an expression is evaluated, it returns a value.

> data Val = VTrue
>          | VFalse
>		   | VInt Int
>   deriving (Show, Eq)

The evaluate function takes an expression and returns a value
The VTrue case has been done for you.
You must complete the other cases.


> evaluate :: Exp -> Val
> evaluate ETrue = VTrue
> evaluate EFalse = VFalse
> evaluate (EInt intVal) = VInt intVal
> evaluate (Eif cond expTrue expFalse) = if (evaluate cond == VTrue)
>											then (evaluate expTrue)
>										else if (evaluate cond == VFalse)
>											then (evaluate expFalse)
>										else error "Invalid expression"
> evaluate (ESucc (EInt n)) = VInt (n+1)
> evaluate (ESucc _) = error "Invalid Expression"
> evaluate (EPred (EInt n)) = VInt (n-1)
> evaluate (EPred _) = error "Invalid Expression"


And here we have a couple of programs to test.
prog1 should evaluate to VTrue and prog2 should evaluate to VFalse

> prog1 = Eif ETrue ETrue EFalse
> prog2 = Eif (Eif ETrue EFalse ETrue) ETrue (Eif ETrue EFalse ETrue)
> prog3 = ESucc (EInt 0)
> prog4 = EPred (EInt 1)

The following lines evaluate the test expressions and display the results.
Note the type of main.  'IO ()' indicates that the function performs IO and returns nothing.
The word 'do' begins a block of code, were you can effectively do sequential statements.
(This is a crude generalization, but we'll talk more about what is going on in this function
when we deal with the great and terrible subject of _monads_.)

> main :: IO ()
> main = do
>   putStrLn $ "Evaluating '" ++ (show prog1) ++ "' results in " ++ (show $ evaluate prog1)
>   putStrLn $ "Evaluating '" ++ (show prog2) ++ "' results in " ++ (show $ evaluate prog2)

Commenting these statements as they could break the code checker, if any
putStrLn $ "Evaluating '" ++ (show prog3) ++ "' results in " ++ (show $ evaluate prog3)
putStrLn $ "Evaluating '" ++ (show prog4) ++ "' results in " ++ (show $ evaluate prog4)

Once you have the evaluate function working
you add in support the expressions 'succ e', 'pred e', and 'zero'.
With this change, it is possible for evaluate to get 'stuck',
e.g. pred true.
For a first pass, simply use the error function in these cases.