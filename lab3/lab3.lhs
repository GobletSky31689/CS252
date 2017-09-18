> import Data.List

Experiment with foldl, foldr, and foldl'

First, implement your own version of the foldl function,
defined as myFoldl

> myFoldl :: (a -> b -> a) -> a -> [b] -> a
> myFoldl _ acc [] = acc
> myFoldl f acc (x:xs) = myFoldl f (f acc x) xs


Next, define a function to reverse a list using foldl.

> myReverse :: [a] -> [a]
> myReverse lst = myFoldl (\acc next -> next:acc) [] lst


Now define your own version of foldr, named myFoldr

> myFoldr :: (a -> b -> b) -> b -> [a] -> b
> myFoldr _ acc [] = acc
> myFoldr f acc (x:xs) = f x $ myFoldr f acc xs


Now try using foldl (the library version, not yours) to sum up the numbers of a large list.
Why is it so slow?

Instead of foldl, try using foldl'.
Why is it faster?
(Read http://www.haskell.org/haskellwiki/Foldr_Foldl_Foldl%27 for some hints)
Ans: In case of foldl', there is no time wasted in creating thunks of lazy variables.

For an extra challenge, try to implement foldl in terms of foldr.
See http://www.haskell.org/haskellwiki/Foldl_as_foldr for details.

> awesomeFoldl :: (a -> b -> a) -> a -> [b] -> a
> awesomeFoldl _ acc [] = acc
> awesomeFoldl f acc lst = myFoldr (flip f) acc $ reverse lst


Next, using the map function, convert every item in a list to its absolute value

> listAbs :: [Integer] -> [Integer]
> listAbs x = map abs x

Finally, write a function that takes a list of Integers and returns the sum of
their absolute values.

> sumAbs :: [Integer] -> Integer
> sumAbs x = myFoldl ((+).abs) 0 x

