module BinaryTree.SearchTree (
  add,
  find,
  contains,
  delete
) where
import           BinaryTree.BinaryTree          (makeLeaf, makeNode)
import           BinaryTree.Internal.BinaryTree (BinaryTree (..))
import           Data.Maybe                     (isJust)

-- Add the given node into the search tree. Replaces if already existing.
add :: (Ord a) => a -> BinaryTree a -> BinaryTree a
add e BEmpty = makeLeaf e
add e (BNode root t1 t2) = case compare e root of
  EQ -> makeNode e t1 t2
  LT -> let t1' = add e t1 in makeNode root t1' t2
  GT -> let t2' = add e t2 in makeNode root t1 t2'


find :: (Ord a) => a -> BinaryTree a -> Maybe a
find = undefined

contains :: (Ord a) => a -> BinaryTree a -> Bool
contains e tree = isJust $ find e tree

delete :: (Ord a) => a -> BinaryTree a -> Maybe (a, BinaryTree a)
delete = undefined
