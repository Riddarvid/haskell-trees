{-# LANGUAGE InstanceSigs #-}

module BinaryTree.BinaryTree (
  BinaryTreeClass(..),
  BinaryTree
) where
import           BinaryTree.Internal.BinaryNode (BinaryNode,
                                                 NodeTree (makeTree, rootNode))
import qualified BinaryTree.Internal.BinaryNode as N
import           BinaryTree.Traversal           (TraverseOrder)
import           Test.QuickCheck                (Arbitrary (arbitrary), Gen,
                                                 frequency, sized)

class (NodeTree t) => BinaryTreeClass t where
  makeEmpty :: t a
  makeEmpty = makeTree N.makeEmpty
  makeNode :: a -> t a -> t a -> t a
  makeNode root lt rt = makeTree $ N.makeNode root ln rn
    where
      ln = rootNode lt
      rn = rootNode rt
  makeLeaf :: a -> t a
  makeLeaf =  makeTree . N.makeLeaf

  leftSubTree :: t a -> Maybe (t a)
  leftSubTree tree = makeTree <$> N.leftSubTree (rootNode tree)
  rightSubTree :: t a -> Maybe (t a)
  rightSubTree tree = makeTree <$> N.rightSubTree (rootNode tree)
  rootData :: t a -> Maybe a
  rootData = N.rootData . rootNode
  isLeaf :: t a -> Bool
  isLeaf = N.isLeaf . rootNode
  isEmpty :: t a -> Bool
  isEmpty = N.isEmpty . rootNode

  traverseTree :: TraverseOrder -> t a -> [(a, Int)]
  traverseTree order tree = N.traverseTree order (rootNode tree)
  numberOfNodes :: t a -> Int
  numberOfNodes = N.numberOfNodes . rootNode
  prettyShow :: Show a => TraverseOrder -> t a -> String
  prettyShow order tree = N.prettyShow order (rootNode tree)

-- Type

newtype BinaryTree a = BT (BinaryNode a)
  deriving (Show)

instance NodeTree BinaryTree where
  rootNode :: BinaryTree a -> BinaryNode a
  rootNode (BT n) = n
  makeTree :: BinaryNode a -> BinaryTree a
  makeTree = BT

instance BinaryTreeClass BinaryTree

-- QickCheck

instance Arbitrary a => Arbitrary (BinaryTree a) where
  arbitrary :: Gen (BinaryTree a)
  arbitrary = treeGen

treeGen :: Arbitrary a => Gen (BinaryTree a)
treeGen = sized treeGen'

treeGen' :: Arbitrary a => Int -> Gen (BinaryTree a)
treeGen' 0 = emptyGen
treeGen' n = frequency [(3, nodeGen n), (1, emptyGen)]

nodeGen :: Arbitrary a => Int -> Gen (BinaryTree a)
nodeGen n = do
  root <- arbitrary
  lt <- treeGen' (n `div` 2)
  rt <- treeGen' (n `div` 2)
  return $ makeNode root lt rt

emptyGen :: Gen (BinaryTree a)
emptyGen = return makeEmpty
