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
import           Data.Maybe                     (catMaybes)

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
traverseTree order tree = foldr addJust [] nodes
  where
    nodes = traverseTreeMaybes order tree
    addJust (Nothing, _) xs = xs
    addJust (Just x, d) xs  = (x, d) : xs

traverseTreeMaybes :: TraverseOrder -> BinaryTree a -> [(Maybe a, Int)]
traverseTreeMaybes = traverseTree' 0

traverseTree' :: Int -> TraverseOrder -> BinaryTree a -> [(Maybe a, Int)]
traverseTree' depth _ BEmpty = [(Nothing, depth)]
traverseTree' depth order rootNode@(BNode e t1 t2) = case order of
  Inorder      -> leftList ++ rootList ++ rightList
  Preorder     -> rootList ++ leftList ++ rightList
  Postorder    -> leftList ++ rightList ++ rootList
  BreadthFirst -> traverseBreadth depth [rootNode]
  where
    leftList = traverseTree' (depth + 1) order t1
    rightList = traverseTree' (depth + 1) order t2
    rootList = [(Just e, depth)]

traverseBreadth :: Int -> [BinaryTree a] -> [(Maybe a, Int)]
traverseBreadth _ [] = []
traverseBreadth depth nodes = current ++ traverseBreadth (depth + 1) childNodes
  where
    current = map ((, depth) . rootData) nodes
    childNodes = concatMap children nodes

children :: BinaryTree a -> [BinaryTree a]
children n = catMaybes [leftSubTree n, rightSubTree n]

numberOfNodes :: BinaryTree a -> Int
numberOfNodes = length . traverseTree Preorder

prettyShow :: Show a => BinaryTree a -> String
prettyShow = concatMap printNode . traverseTreeMaybes Preorder
  where
    printNode (val, depth) = let
      dots = concat $ replicate depth ".\t"
      in dots ++ show val ++ "\n"
