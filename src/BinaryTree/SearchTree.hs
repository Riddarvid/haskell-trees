{-# LANGUAGE InstanceSigs #-}

module BinaryTree.SearchTree (
  SearchTreeClass(..),
  SearchTree
) where
import           BinaryTree.BinaryTree          (BinaryTreeClass (..))
import           BinaryTree.Internal.BinaryNode (BinaryNode)
import           BinaryTree.Internal.Defaults   (NodeTree (..))
import qualified BinaryTree.Internal.Defaults   as D
import qualified BinaryTree.Internal.SearchNode as N
import           BinaryTree.Traversal           (TraverseOrder)
import           Data.Maybe                     (isJust)

class SearchTreeClass t where
  add :: (Ord a) => a -> t a -> t a
  find :: (Ord a) => a -> t a -> Maybe a
  contains :: (Ord a) => a -> t a -> Bool
  contains e = isJust . find e
  delete :: (Ord a) => a -> t a -> Maybe (a, t a)
  fromList :: (Ord a) => [a] -> t a

newtype SearchTree a = ST (BinaryNode a)

instance NodeTree SearchTree where
  rootNode :: SearchTree a -> BinaryNode a
  rootNode (ST n) = n
  makeTree :: BinaryNode a -> SearchTree a
  makeTree = ST

instance BinaryTreeClass SearchTree where
  makeEmpty :: SearchTree a
  makeEmpty = D.makeEmpty
  makeNode :: a -> SearchTree a -> SearchTree a -> SearchTree a
  makeNode = D.makeNode
  makeLeaf :: a -> SearchTree a
  makeLeaf = D.makeLeaf
  leftSubTree :: SearchTree a -> Maybe (SearchTree a)
  leftSubTree = D.leftSubTree
  rightSubTree :: SearchTree a -> Maybe (SearchTree a)
  rightSubTree = D.rightSubTree
  rootData :: SearchTree a -> Maybe a
  rootData = D.rootData
  isLeaf :: SearchTree a -> Bool
  isLeaf = D.isLeaf
  traverseTree :: TraverseOrder -> SearchTree a -> [(a, Int)]
  traverseTree = D.traverseTree
  numberOfNodes :: SearchTree a -> Int
  numberOfNodes = D.numberOfNodes
  prettyShow :: Show a => TraverseOrder -> SearchTree a -> String
  prettyShow = D.prettyShow

instance SearchTreeClass SearchTree where
  add :: Ord a => a -> SearchTree a -> SearchTree a
  add e = makeTree . N.add e . rootNode
  find :: Ord a => a -> SearchTree a -> Maybe a
  find e = N.find e . rootNode
  delete :: Ord a => a -> SearchTree a -> Maybe (a, SearchTree a)
  delete e tree = case N.delete e (rootNode tree) of
    Nothing          -> Nothing
    Just (e', root') -> Just (e', makeTree root')
  fromList :: Ord a => [a] -> SearchTree a
  fromList = makeTree . N.fromList
