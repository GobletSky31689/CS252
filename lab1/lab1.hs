-- The following function should take a list of Integers
-- and return the largest.
-- THE DEFINITION MUST BE RECURSIVE.
maxNum :: [Integer] -> Integer
maxNum _ = error "Not implemented"
maxNum [x] = x
maxNum (x:xs) = maxNum ([y | y <- xs, y > x]++[x])

-- maxNum (x:xs) = if x > largest
-- 					then x
-- 					else largest
-- 				where largest = maxNum xs







-- Do the game fizzbuzz (http://en.wikipedia.org/wiki/Fizz_buzz).
-- Return a string counting from 1 to the specified number.
-- Replace numbers divisible by 3 with "fizz" and numbers divisible
-- by 5 with "buzz".  If a number is divisible by both 3 and 5,
-- replace it with "fizzbuzz".
fizzbuzz :: Int -> String
fizzbuzz 1 = "1"
fizzbuzz x = do
    if mod x 15 == 0 then fizzbuzz (x-1) ++ " fizzbuzz"
    else if mod x 3 == 0 then fizzbuzz (x-1) ++ " fizz"
    else if mod x 5 == 0 then fizzbuzz (x-1) ++ " buzz"
    else fizzbuzz (x-1) ++ " " ++ show x

 -- Using guards above is the more Haskelly way!!


-- Download JSON.hs and jsonDriver.hs from the course website.
-- In JSON.hs, implement the JObject case of the toString function.  
-- Compile and run jsonDriver.hs
-- and verify that your results look to be correct.
-- The error comes out as:
-- [jsonDriver.hs: JSON.hs:(23,1)-(28,52): Non-exhaustive patterns in function toString

toString (JObject o)    = "{" ++  
                              (unwords [ "\"" ++ (fst x) ++ "\"" ++ ":" ++ (toString (snd x)) ++ "," | x<-init o]) ++  
                              "\"" ++ (fst (last o)) ++ "\"" ++ ":" ++ (toString (snd (last o))) ++
                           "}"



