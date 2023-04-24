{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TupleSections #-}

module BinaryTree.Internal.BinaryNode (
  -- Data types
  BinaryNode(..),
  -- Constructors
  makeEmpty,
  makeNode,
  makeLeaf,
  -- Queries
  leftSubTree,
  rightSubTree,
  rootData,
  isLeaf,
  -- Traversal
  traverseTree,
  -- Utils
  numberOfNodes,
  prettyShow
) where
import           BinaryTree.Traversal (TraverseOrder (..))
import           Data.Maybe           (catMaybes)

data BinaryNode a = BNode a (BinaryNode a) (BinaryNode a) | BEmpty

instance Functor BinaryNode where
  fmap :: (a -> b) -> BinaryNode a -> BinaryNode b
  fmap _ BEmpty          = BEmpty
  fmap f (BNode e n1 n2) = BNode (f e) (fmap f n1) (fmap f n2)

-- Constructors

makeEmpty :: BinaryNode a
makeEmpty = BEmpty

makeNode :: a -> BinaryNode a -> BinaryNode a -> BinaryNode a
makeNode = BNode

makeLeaf :: a -> BinaryNode a
makeLeaf e = makeNode e makeEmpty makeEmpty

-- Queries

leftSubTree :: BinaryNode a -> Maybe (BinaryNode a)
leftSubTree BEmpty        = Nothing
leftSubTree (BNode _ t _) = Just t

rightSubTree :: BinaryNode a -> Maybe (BinaryNode a)
rightSubTree BEmpty        = Nothing
rightSubTree (BNode _ _ t) = Just t

rootData :: BinaryNode a -> Maybe a
rootData BEmpty        = Nothing
rootData (BNode e _ _) = Just e

isLeaf :: BinaryNode a -> Bool
isLeaf BEmpty                  = False
isLeaf (BNode _ BEmpty BEmpty) = True
isLeaf _                       = False

-- Traversal

traverseTree :: TraverseOrder -> BinaryNode a -> [(a, Int)]
traverseTree order tree = foldr addJust [] nodes
  where
    nodes = traverseTreeMaybes order tree
    addJust (Nothing, _) xs = xs
    addJust (Just x, d) xs  = (x, d) : xs

traverseTreeMaybes :: TraverseOrder -> BinaryNode a -> [(Maybe a, Int)]
traverseTreeMaybes = traverseTree' 0

traverseTree' :: Int -> TraverseOrder -> BinaryNode a -> [(Maybe a, Int)]
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

traverseBreadth :: Int -> [BinaryNode a] -> [(Maybe a, Int)]
traverseBreadth _ [] = []
traverseBreadth depth nodes = current ++ traverseBreadth (depth + 1) childNodes
  where
    current = map ((, depth) . rootData) nodes
    childNodes = concatMap children nodes

children :: BinaryNode a -> [BinaryNode a]
children n = catMaybes [leftSubTree n, rightSubTree n]

-- Utils

numberOfNodes :: BinaryNode a -> Int
numberOfNodes = length . traverseTree Preorder

prettyShow :: Show a => TraverseOrder -> BinaryNode a -> String
prettyShow order = concatMap printNode . traverseTreeMaybes order
  where
    printNode (val, depth) = let
      dots = concat $ replicate depth ".\t"
      in dots ++ show val ++ "\n"
