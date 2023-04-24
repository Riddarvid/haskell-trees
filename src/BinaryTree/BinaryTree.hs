{-# LANGUAGE TupleSections #-}

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
  numberOfNodes,
  prettyShow
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

traverseTree :: TraverseOrder -> BinaryTree a -> [(a, Int)]
traverseTree = traverseTree' 0

traverseTree' :: Int -> TraverseOrder -> BinaryTree a -> [(a, Int)]
traverseTree' _ _ BEmpty = []
traverseTree' depth order rootNode@(BNode e t1 t2) = case order of
  Inorder      -> leftList ++ rootList ++ rightList
  Preorder     -> rootList ++ leftList ++ rightList
  Postorder    -> leftList ++ rightList ++ rootList
  BreadthFirst -> traverseBreadth depth [rootNode]
  where
    leftList = traverseTree' (depth + 1) order t1
    rightList = traverseTree' (depth + 1) order t2
    rootList = [(e, depth)]

traverseBreadth :: Int -> [BinaryTree a] -> [(a, Int)]
traverseBreadth _ [] = []
traverseBreadth depth nodes = current ++ traverseBreadth (depth + 1) childNodes
  where
    current = map (, depth) $ mapMaybe rootData nodes
    childNodes = concatMap children nodes

children :: BinaryTree a -> [BinaryTree a]
children n = catMaybes [leftSubTree n, rightSubTree n]

numberOfNodes :: BinaryTree a -> Int
numberOfNodes = length . traverseTree Preorder

prettyShow :: Show a => BinaryTree a -> String
prettyShow = concatMap printNode . traverseTree Preorder
  where
    printNode (val, depth) = let
      dots = concat $ replicate depth ".\t"
      in dots ++ show val ++ "\n"
