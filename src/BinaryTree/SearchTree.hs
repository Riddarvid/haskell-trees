{-# LANGUAGE InstanceSigs #-}

module BinaryTree.SearchTree (
  SearchTreeClass(..),
  SearchTree
) where
import           BinaryTree.BinaryTree          (BinaryTreeClass (..))
import           BinaryTree.Internal.BinaryNode (BinaryNode,
                                                 NodeTree (makeTree, rootNode))
import qualified BinaryTree.Internal.SearchNode as N
import           Data.Maybe                     (isJust)

class (NodeTree t) => SearchTreeClass t where
  add :: (Ord a) => a -> t a -> t a
  add e = makeTree . N.add e . rootNode
  find :: (Ord a) => a -> t a -> Maybe a
  find e = N.find e . rootNode
  contains :: (Ord a) => a -> t a -> Bool
  contains e = isJust . find e
  delete :: (Ord a) => a -> t a -> Maybe (a, t a)
  delete e tree = case N.delete e (rootNode tree) of
    Nothing          -> Nothing
    Just (e', root') -> Just (e', makeTree root')
  fromList :: (Ord a) => [a] -> t a
  fromList = makeTree . N.fromList

newtype SearchTree a = ST (BinaryNode a)

instance NodeTree SearchTree where
  rootNode :: SearchTree a -> BinaryNode a
  rootNode (ST n) = n
  makeTree :: BinaryNode a -> SearchTree a
  makeTree = ST

instance BinaryTreeClass SearchTree

instance SearchTreeClass SearchTree
