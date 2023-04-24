module BinaryTree.Internal.SearchNode (
  add,
  find,
  delete,
  fromList
) where
import           BinaryTree.Internal.BinaryNode (BinaryNode (..), makeEmpty,
                                                 makeLeaf, makeNode)

-- Add the given node into the search tree. Replaces if already existing.
add :: (Ord a) => a -> BinaryNode a -> BinaryNode a
add e BEmpty = makeLeaf e
add e (BNode root t1 t2) = case compare e root of
  EQ -> makeNode e t1 t2
  LT -> let t1' = add e t1 in makeNode root t1' t2
  GT -> let t2' = add e t2 in makeNode root t1 t2'

find :: (Ord a) => a -> BinaryNode a -> Maybe a
find _ BEmpty = Nothing
find e (BNode root n1 n2) = case compare e root of
  EQ -> Just root
  LT -> find e n1
  GT -> find e n2

-- If the element is in the tree, return just the tree and the deleted element.
-- Otherwise return Nothing.
delete :: (Ord a) => a -> BinaryNode a -> Maybe (a, BinaryNode a)
delete _ BEmpty = Nothing
delete e rootNode@(BNode root lt rt) = case compare e root of
  EQ -> Just (root, deleteRoot rootNode)
  LT -> do
    (deleted, lt') <- delete e lt
    return (deleted, makeNode root lt' rt)
  GT -> do
    (deleted, rt') <- delete e rt
    return (deleted, makeNode root lt rt')

deleteRoot :: BinaryNode a -> BinaryNode a
deleteRoot BEmpty                  = error "Delete root of empty tree"
deleteRoot (BNode _ BEmpty BEmpty) = BEmpty -- No children - delete
deleteRoot (BNode _ BEmpty rt)     = rt     -- One child - link past
deleteRoot (BNode _ lt BEmpty)     = lt     -- One child - link past
deleteRoot (BNode _ lt rt) = makeNode maxVal lt' rt
  where
    (maxVal, lt') = deleteMax lt

deleteMax :: BinaryNode a -> (a, BinaryNode a)
deleteMax BEmpty = error "Delete max of empty tree"
deleteMax (BNode root lt BEmpty) = (root, lt) -- Max element found
deleteMax (BNode root lt rt) = (maxVal, makeNode root lt rt')
  where
    (maxVal, rt') = deleteMax rt

-- Creates search tree containing the items of the list. Items are inserted in order from
-- left to right.
fromList :: Ord a => [a] -> BinaryNode a
fromList = foldl (flip add) makeEmpty
