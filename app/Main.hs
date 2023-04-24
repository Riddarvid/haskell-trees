module Main (main) where

import           BinaryTree.BinaryTree (BinaryTree)
import qualified BinaryTree.BinaryTree as BT
import           BinaryTree.Traversal  (TraverseOrder (..))


main :: IO ()
main = do
  putStrLn "Normal binary tree, not sorted:"
  putStrLn $ BT.prettyShow Inorder testTree
  putStrLn "Inorder"
  print $ BT.traverseTree Inorder testTree
  putStrLn "Preorder"
  print $ BT.traverseTree Preorder testTree
  putStrLn "Postorder"
  print $ BT.traverseTree Postorder testTree
  putStrLn "Breadth first"
  print $ BT.traverseTree BreadthFirst testTree
  putStrLn "Number of nodes"
  print $ BT.numberOfNodes testTree

  {-putStrLn "Search tree pretty:"
  putStrLn $ BT.prettyShow Inorder testSearchTree
  putStrLn "Search tree Inorder:"
  print $ BT.traverseTree Inorder testSearchTree-}

testTree :: BinaryTree Int
testTree = BT.makeNode 5
  (BT.makeNode 2 (BT.makeLeaf 7) BT.makeEmpty)
  (BT.makeNode 3 (BT.makeLeaf 57) (BT.makeLeaf 12))

{-testSearchTree :: SearchTree Int
testSearchTree = ST.fromList [5, 3, 7, 57, 12, 2]-}
