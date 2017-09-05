inc :: Int -> Int
inc x | x < 0 = error "inc is not supported for negative numbers"
inc x = x + 1

incFst p = (1 + fst p, snd p)
--incFst (x,y) = (x+1,y)


