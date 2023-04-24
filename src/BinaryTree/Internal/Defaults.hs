module BinaryTree.Internal.Defaults (
  NodeTree(..),
  makeEmpty,
  makeNode,
  makeLeaf,
  leftSubTree,
  rightSubTree,
  rootData,
  isLeaf,
  traverseTree,
  numberOfNodes,
  prettyShow
) where

import           BinaryTree.Internal.BinaryNode (BinaryNode)
import qualified BinaryTree.Internal.BinaryNode as N
import           BinaryTree.Traversal           (TraverseOrder)

class NodeTree t where
  rootNode :: t a -> BinaryNode a
  makeTree :: BinaryNode a -> t a

makeEmpty :: (NodeTree t) => t a
makeEmpty = makeTree N.makeEmpty

makeNode :: (NodeTree t) => a -> t a -> t a -> t a
makeNode root lt rt = makeTree $ N.makeNode root ln rn
  where
    ln = rootNode lt
    rn = rootNode rt

makeLeaf :: (NodeTree t) => a -> t a
makeLeaf =  makeTree . N.makeLeaf

leftSubTree :: (NodeTree t) => t a -> Maybe (t a)
leftSubTree tree = makeTree <$> N.leftSubTree (rootNode tree)

rightSubTree :: (NodeTree t) => t a -> Maybe (t a)
rightSubTree tree = makeTree <$> N.rightSubTree (rootNode tree)

rootData :: (NodeTree t) => t a -> Maybe a
rootData = N.rootData . rootNode

isLeaf :: (NodeTree t) => t a -> Bool
isLeaf = N.isLeaf . rootNode

traverseTree :: (NodeTree t) => TraverseOrder -> t a -> [(a, Int)]
traverseTree order tree = N.traverseTree order (rootNode tree)

numberOfNodes :: (NodeTree t) => t a -> Int
numberOfNodes = N.numberOfNodes . rootNode

prettyShow :: (NodeTree t, Show a) => TraverseOrder -> t a -> String
prettyShow order tree = N.prettyShow order (rootNode tree)
