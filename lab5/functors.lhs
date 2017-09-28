> data Tree v =
>     Empty
>   | Node v (Tree v) (Tree v)
>   deriving (Show)

> findT :: Ord v => v -> Tree v -> Maybe v
> findT _ Empty = Nothing
> findT v (Node val left right) =
>   if val == v then
>     Just val
>   else if v < val then
>     findT v left
>   else
>     findT v right


> instance Functor Tree where 
> 	fmap f Empty = Empty
>	fmap f (Node val left right) =  Node (f val) (fmap f left) (fmap f right) 


> data Val v = NothingToSee
>		| VInt v
> 	deriving (Show)

> instance Functor Val where
>	fmap f NothingToSee = NothingToSee
> 	fmap f (VInt a) = VInt (f a)


Your job is to add support for fmap to this tree, so that the following call works:

fmap ((+1).(+1)) (Val 3)


fmap ((+1).(+1)) (Node 3 (Node 1 Empty Empty) (Node 7 (Node 4 Empty Empty) Empty))

