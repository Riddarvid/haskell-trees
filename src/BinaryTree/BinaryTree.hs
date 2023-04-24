{-# LANGUAGE InstanceSigs #-}

module BinaryTree.BinaryTree (
  BinaryTreeClass(..),
  BinaryTree
) where
import           BinaryTree.Internal.BinaryNode (BinaryNode)
import           BinaryTree.Internal.Defaults   (NodeTree)
import qualified BinaryTree.Internal.Defaults   as D
import           BinaryTree.Traversal           (TraverseOrder)

class BinaryTreeClass t where
  makeEmpty :: t a
  makeNode :: a -> t a -> t a -> t a
  makeLeaf :: a -> t a

  leftSubTree :: t a -> Maybe (t a)
  rightSubTree :: t a -> Maybe (t a)
  rootData :: t a -> Maybe a
  isLeaf :: t a -> Bool

  traverseTree :: TraverseOrder -> t a -> [(a, Int)]
  numberOfNodes :: t a -> Int
  prettyShow :: Show a => TraverseOrder -> t a -> String

newtype BinaryTree a = BT (BinaryNode a)

instance NodeTree BinaryTree where
  rootNode :: BinaryTree a -> BinaryNode a
  rootNode (BT n) = n
  makeTree :: BinaryNode a -> BinaryTree a
  makeTree = BT

instance BinaryTreeClass BinaryTree where
  makeEmpty :: BinaryTree a
  makeEmpty = D.makeEmpty
  makeNode :: a -> BinaryTree a -> BinaryTree a -> BinaryTree a
  makeNode = D.makeNode
  makeLeaf :: a -> BinaryTree a
  makeLeaf = D.makeLeaf
  leftSubTree :: BinaryTree a -> Maybe (BinaryTree a)
  leftSubTree = D.leftSubTree
  rightSubTree :: BinaryTree a -> Maybe (BinaryTree a)
  rightSubTree = D.rightSubTree
  rootData :: BinaryTree a -> Maybe a
  rootData = D.rootData
  isLeaf :: BinaryTree a -> Bool
  isLeaf = D.isLeaf
  traverseTree :: TraverseOrder -> BinaryTree a -> [(a, Int)]
  traverseTree = D.traverseTree
  numberOfNodes :: BinaryTree a -> Int
  numberOfNodes = D.numberOfNodes
  prettyShow :: Show a => TraverseOrder -> BinaryTree a -> String
  prettyShow = D.prettyShow
