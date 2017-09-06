--This works, but it is not generic
data TreeStringInt =
    EmptyTSI
  | NodeTSI String Int TreeStringInt TreeStringInt
  deriving (Show)

findTsi :: String -> TreeStringInt -> Maybe Int
findTsi _ EmptyTSI = Nothing
findTsi s (NodeTSI key i left right) =
  if key == s then
    Just i
  else if s < key then
    findTsi s left
  else
    findTsi s right



--Using type parameters, we can create a BST in a more generic form
data Tree k v =
    Empty
  | Node k v (Tree k v) (Tree k v)
  deriving (Show)

--findT :: k -> Tree k v -> Maybe v
findT :: Ord k => k -> Tree k v -> Maybe v
findT _ Empty = Nothing
findT s (Node key val left right) =
  if key == s then
    Just val
  else if s < key then
    findT s left
  else
    findT s right


