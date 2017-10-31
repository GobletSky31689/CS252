data Expression = EInt Integer
				| Add Expression Expression
				| Divide Expression Expression
				deriving (Show)


evaluate :: Expression -> Either String Integer
evaluate (EInt x) = return x
evaluate (Add e1 e2) = do
	v1 <- evaluate e1
	v2 <- evaluate e2
	return $ v1 + v2
evaluate (Divide e1 e2) = do
	v1 <- evaluate e1
	v2 <- evaluate e2
	if v2 == 0
		then Left "Cannot Divide by zero"
	else 
		return $ v1 `div` v2


-- evaluate (Add e1 e2) = (evaluate e1)>>= \x -> (evaluate e2)>>= \y -> return $ x + y
-- 
-- evaluate (Divide e1 e2) = (evaluate e1)>>= \x -> (evaluate e2)>>= \y -> if y==0 
-- 																			then Left "Cannot divide by zero"
-- 																		else 
-- 																			return $ x `div` y



evalAndPrint e = case evaluate e of 
				Left s -> error $ "ERROR: " ++ s
				Right i -> print $ "Result: " ++ show i


main = do
		evalAndPrint (Add (EInt 5) (Add (EInt 2) (EInt 4)))
		evalAndPrint (Divide (EInt 3) (EInt 2))
		evalAndPrint (Divide (EInt 1) (EInt 0))


