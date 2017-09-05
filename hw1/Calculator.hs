-- This is a simple parser for testing out BigNum.
-- Note that there are much better parsers, which
-- we will explore in another class.

import BigNum

-- Crude way of handling whitespace: make sure
-- that all ops are surrounded by whitespace.
addSpace :: String -> String
addSpace []       = ""
addSpace ('+':xs) = " + " ++ addSpace xs
addSpace ('-':xs) = " - " ++ addSpace xs
addSpace ('*':xs) = " * " ++ addSpace xs
addSpace ('^':xs) = " ^ " ++ addSpace xs
addSpace (x:xs)   = x : addSpace xs

calculate :: String -> BigNum -> BigNum -> BigNum
calculate "+" b1 b2 = bigAdd b1 b2
calculate "-" b1 b2 = bigSubtract b1 b2
calculate "*" b1 b2 = bigMultiply b1 b2
calculate "^" b1 b2 = bigPowerOf b1 b2

main :: IO ()
main = do
  line <- getLine
  if null line
    then return ()
    else do
      case words $ addSpace line of
          exp1:op:exp2:[] -> putStrLn $ prettyPrint $ calculate op big1 big2
              where big1 = stringToBigNum exp1
                    big2 = stringToBigNum exp2
          exp:[] -> putStrLn $ show $ stringToBigNum exp
          _ -> putStrLn "Only simply binary expressions are supported"
      main

