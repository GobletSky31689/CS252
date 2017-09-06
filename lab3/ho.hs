import qualified Data.Map as Map

m = Map.empty
m' = Map.insert 3 "hello" m
s = case (Map.lookup 3 m') of
      Just s' -> s'
      Nothing -> error "not found"


addNums x y = x + y

inc :: Int -> Int
inc = addNums 1

incList :: [Int] -> [Int]
incList [] = []
incList (x:xs) = inc x : (incList xs)

decList :: [Int] -> [Int]
decList [] = []
decList (x:xs) = (x-1) : (decList xs)


applyFun2List :: (a->b) -> [a] -> [b]
applyFun2List _ [] = []
applyFun2List f (x:xs) = (f x) : (applyFun2List f xs)

incList' = applyFun2List inc
--incList' = map inc


removeNegatives :: [Integer] -> [Integer]
removeNegatives [] = []
removeNegatives (x:xs) =
  let rest = removeNegatives xs in
    if x >= 0 then
      x : rest
    else
      rest

--filter
removeBadElems :: (a -> Bool) -> [a] -> [a]
removeBadElems _ [] = []
removeBadElems f (x:xs) =
  let rest = removeBadElems f xs in
    if f x then
      x : rest
    else
      rest



addListOfNums :: Num a => a -> [a] -> a
addListOfNums accum [] = accum
addListOfNums accum (x:xs) = addListOfNums (accum + x) xs

decListOfNums :: Num a => a -> [a] -> a
decListOfNums accum [] = accum
decListOfNums accum (x:xs) = addListOfNums (accum - x) xs

addListOfNumStrings :: Int -> [String] -> Int
addListOfNumStrings accum [] = accum
addListOfNumStrings accum (s:xs) = addListOfNumStrings (accum + (read s)) xs


--foldl
foldTogether :: (a -> b -> a) -> a -> [b] -> a
foldTogether _ accum [] = accum
foldTogether f accum (x:xs) = foldTogether f (f accum x) xs

addListOfNums' :: Num a => [a] -> a
addListOfNums' = foldTogether (+) 0





