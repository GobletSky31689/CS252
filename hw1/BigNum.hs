{-
  Name: Ujjawal Garg
  Class: CS 252
  Assigment: HW1
  Date: 5th Sep 2017
  Description: This module adds support for arithmetic operations on large no. without using unbounded Integers
-}

module BigNum (
  BigNum,
  bigAdd,
  bigSubtract,
  bigMultiply,
  bigEq,
  bigDec,
  bigPowerOf,
  prettyPrint,
  stringToBigNum,
) where

type Block = Int -- An Int from 0-999

type BigNum = [Block]

maxblock = 1000

bigAdd :: BigNum -> BigNum -> BigNum
bigAdd x y = bigAdd' x y 0

bigAdd' :: BigNum -> BigNum -> Block -> BigNum
bigAdd' [] [] 0 = []
bigAdd' [] [] z = [z]
bigAdd' (x:xs) (y:ys) z = if sum<maxblock 
                            then sum: (bigAdd xs ys) 
                          else 
                            result: (bigAdd' xs ys carry)
                          where 
                            sum = x+y+z
                            result = mod sum maxblock
                            carry = quot sum maxblock
bigAdd' x [] z = bigAdd' x [0] z
bigAdd' [] y z = bigAdd' [0] y z
-- bigAdd' _ _ _ = error "Your code here"

lessThan :: BigNum -> BigNum -> Bool
lessThan [] [] = False
lessThan [] (y:[]) = True
lessThan (x:[]) [] = False
lessThan [x] [y] = x<y
lessThan a@(x:xs) b@(y:ys) =  if length a < length b 
                                then True
                              else if length a > length b 
                                then False
                              else 
                                x<y || (x==y && xs `lessThan` ys)

bigSubtract :: BigNum -> BigNum -> BigNum
bigSubtract x y = if length x < length y || (x `lessThan` y)
                    then error "Negative numbers not supported"
                  else reverse $ stripLeadingZeroes $ reverse result
                    where result = bigSubtract' x y 0

stripLeadingZeroes :: BigNum -> BigNum
stripLeadingZeroes (0:[]) = [0]
stripLeadingZeroes (0:xs) = stripLeadingZeroes xs
stripLeadingZeroes xs = xs

-- Negative numbers are not supported, so you may throw an error in these cases
bigSubtract' :: BigNum -> BigNum -> Block -> BigNum
bigSubtract' [x] [y] z = [x - z - y]
bigSubtract' [x] [] z = bigSubtract' [x] [0] z
bigSubtract' x [] z = bigSubtract' x [0] z
bigSubtract' (x:xs) (y:ys) z = if x-z>=y 
                                  then (x-z-y): bigSubtract' xs ys 0
                                else 
                                  (maxblock+x-z-y):bigSubtract' xs ys 1
-- bigSubtract' _ _ _ = error "Your code here"

bigEq :: BigNum -> BigNum -> Bool
bigEq [] [] = True
bigEq [x] [] = False
bigEq [] [y] = False
bigEq (x:xs) (y:ys) = x==y && xs `bigEq` ys

bigDec :: BigNum -> BigNum
bigDec x = bigSubtract x [1]

bigMultiply :: BigNum -> BigNum -> BigNum
bigMultiply x [y] = bigMultiply' x [y] 0
bigMultiply x (y:ys) = bigAdd (bigMultiply' x [y] 0) ([0]++(bigMultiply x ys))
-- bigMultiply _ _ = error "Your code here"

bigMultiply' :: BigNum -> BigNum -> Block -> BigNum
bigMultiply' [] _ 0 = []
bigMultiply' [] _ z = [z]
bigMultiply' _ [] _ = []
bigMultiply' _ [0] _ = [0]
bigMultiply' (x:xs) [y] z =  if prod < maxblock 
                              then prod : bigMultiply' xs [y] 0
                            else (prod `mod` maxblock): bigMultiply' xs [y] carry
                            where 
                              prod = x*y+z
                              carry = prod `quot` maxblock

bigPowerOf :: BigNum -> BigNum -> BigNum
bigPowerOf _ [0] = [1]
bigPowerOf x [1] = x
bigPowerOf x y = bigMultiply x (bigPowerOf x (bigDec y))
-- bigPowerOf _ _ = error "Your code here"

prettyPrint :: BigNum -> String
prettyPrint [] = ""
prettyPrint xs = show first ++ prettyPrint' rest
  where (first:rest) = reverse xs

prettyPrint' :: BigNum -> String
prettyPrint' [] = ""
prettyPrint' (x:xs) = prettyPrintBlock x ++ prettyPrint' xs

prettyPrintBlock :: Int -> String
prettyPrintBlock x | x < 10     = ",00" ++ show x
                   | x < 100    = ",0" ++ show x
                   | otherwise  = "," ++ show x

stringToBigNum :: String -> BigNum
stringToBigNum "0" = [0]
stringToBigNum s = stringToBigNum' $ reverse s

stringToBigNum' :: String -> BigNum
stringToBigNum' [] = []
stringToBigNum' s | length s <= 3 = read (reverse s) : []
stringToBigNum' (a:b:c:rest) = block : stringToBigNum' rest
  where block = read $ c:b:a:[]


