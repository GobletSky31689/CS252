Below we have some mathematical binary arguments that you may recognize from homework 2.

> data Binop =
>     Plus     -- +  :: Int  -> Int  -> Int
>   | Minus    -- -  :: Int  -> Int  -> Int
>   | Times    -- *  :: Int  -> Int  -> Int
>   | Divide   -- /  :: Int  -> Int  -> Int
>   deriving (Show)

applyOp performs these operations, but unlike in the homework,
you now must consider errors (represented by 'Nothing').

> applyOp :: Binop -> Maybe Int -> Maybe Int -> Maybe Int

Plus is done for you.  Notice how code must check for 'Nothing'
for each operand.

> applyOp Plus mi mj =
>   case mi of
>     Nothing -> Nothing
>     Just i ->
>       case mj of
>         Nothing -> Nothing
>         Just j -> Just $ i + j

Minus is also done for you.  This case **does** use monads,
but without the do syntax.

> applyOp Minus mi mj =
>   mi >>= (\i -> mj >>= (\j -> Just $ i - j))

Implement Times and Divide.  Try the Times case without using bind (>>=).

> applyOp Times mi mj = case mi of
>							Nothing -> Nothing
>							Just i -> case mj of
>									Just j -> Just $ i * j

For the Divide case, use bind (>>=) as we did for Minus.
On an attempt to divide by 0, return Nothing as the answer.

> applyOp Divide mi mj =  mi >>= (\i -> mj >>= (\j -> if (j /= 0) then (Just (i `div` j)) else Nothing))

The following test cases will help you verify your changes.

> testapp1 = applyOp Minus (applyOp Times (Just 3) (Just 4)) $ applyOp Divide (Just 8) (Just 2)
> testapp2 = applyOp Minus (applyOp Times (Just 3) (Just 4)) $ applyOp Divide (Just 8) (applyOp Plus (Just 3) (Just (-3)))


Now implement applyOp', which implements all methods using the do syntax.
The Plus case is done for you once again.  Be sure to check for zero with Divide.

> applyOp' :: Binop -> Maybe Int -> Maybe Int -> Maybe Int
> applyOp' Plus mi mj = do
>   i <- mi
>   j <- mj
>   return $ i + j
> applyOp' Minus mi mj = do
>   i <- mi
>   j <- mj
>   return $ i - j
> applyOp' Times mi mj = do
>   i <- mi
>   j <- mj
>   return $ i * j
> applyOp' Divide mi mj = do
>   i <- mi
>   j <- mj
>   if (j==0) then (Nothing) else (return $ i `div` j)

More test cases:

> testapp1' = applyOp' Minus (applyOp Times (Just 3) (Just 4)) $ applyOp Divide (Just 8) (Just 2)
> testapp2' = applyOp' Minus (applyOp Times (Just 3) (Just 4)) $ applyOp Divide (Just 8) (applyOp Plus (Just 3) (Just (-3)))


Finally, note the following function for incrementing and decrementing ints.

> mincr :: Int -> Maybe Int
> mincr i = Just $ i + 1

> mdecr :: Int -> Maybe Int
> mdecr i = Just $ i - 1

Experiment with these functions and the >>= syntax.
Here is one example:

> testIncDec = Just 7 >>= mincr >>= mincr >>= mincr >>= mdecr

Does bind seem more natural in this case than using do?  Why or why not?

> testIncDecDo = do
>   i <- Just 7 
>   j <- mincr i
>   k <- mincr j
>   l <- mincr k
>   mdecr l

Yes, Bind syntax looks more natural as it seems similar to (.) chaining.