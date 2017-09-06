fact :: Integer -> Integer
fact 1 = 1
fact n = n * (fact $ n - 1)


fact' :: Integer -> Integer -> Integer
fact' 0 acc = acc
fact' n acc = fact' (n - 1) (n * acc)

fact2 :: Integer -> Integer -> Integer
fact2 n acc = if n == 0
    then acc
    else fact2 (n - 1) (n * acc)

