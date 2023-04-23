module BinaryTree.BinaryTree (
  BinaryTree,
  TraverseOrder(..),
  makeEmpty,
  makeLeaf,
  makeNode,
  leftSubTree,
  rightSubTree,
  rootData,
  isLeaf,
  traverseTree,
  numberOfNodes
) where

import           BinaryTree.Internal.BinaryTree (BinaryTree (..))
import           Data.Maybe                     (catMaybes, mapMaybe)

data TraverseOrder = Inorder | Preorder | Postorder | BreadthFirst

makeEmpty :: BinaryTree a
makeEmpty = BEmpty

makeNode :: a -> BinaryTree a -> BinaryTree a -> BinaryTree a
makeNode = BNode

makeLeaf :: a -> BinaryTree a
makeLeaf e = makeNode e makeEmpty makeEmpty

leftSubTree :: BinaryTree a -> Maybe (BinaryTree a)
leftSubTree BEmpty        = Nothing
leftSubTree (BNode _ t _) = Just t

rightSubTree :: BinaryTree a -> Maybe (BinaryTree a)
rightSubTree BEmpty        = Nothing
rightSubTree (BNode _ _ t) = Just t

rootData :: BinaryTree a -> Maybe a
rootData BEmpty        = Nothing
rootData (BNode e _ _) = Just e

isLeaf :: BinaryTree a -> Bool
isLeaf BEmpty                  = False
isLeaf (BNode _ BEmpty BEmpty) = True
isLeaf _                       = False

traverseTree :: TraverseOrder -> BinaryTree a -> [a]
traverseTree _ BEmpty = []
traverseTree order rootNode@(BNode e t1 t2) = case order of
  Inorder      -> leftList ++ rootList ++ rightList
  Preorder     -> rootList ++ leftList ++ rightList
  Postorder    -> leftList ++ rightList ++ rootList
  BreadthFirst -> traverseBreadth [rootNode]
  where
    leftList = traverseTree order t1
    rightList = traverseTree order t2
    rootList = [e]

traverseBreadth :: [BinaryTree a] -> [a]
traverseBreadth [] = []
traverseBreadth nodes = mapMaybe rootData nodes ++ traverseBreadth childNodes
  where
    childNodes = concatMap children nodes

children :: BinaryTree a -> [BinaryTree a]
children n = catMaybes [leftSubTree n, rightSubTree n]

numberOfNodes :: BinaryTree a -> Int
numberOfNodes = length . traverseTree Preorder
