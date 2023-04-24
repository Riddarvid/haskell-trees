import           BinaryTree.BinaryTree (BinaryTree,
                                        BinaryTreeClass (isEmpty, leftSubTree, numberOfNodes, rightSubTree))
import           Data.Maybe            (fromJust)
import           Test.QuickCheck       (Property, quickCheck, (===), (==>))
main :: IO ()
main = do
  quickCheck propSubTreeSize

type TreeContent = Int

propSubTreeSize :: BinaryTree TreeContent -> Property
propSubTreeSize tree = (not . isEmpty) tree ==> treeNodes === 1 + leftNodes + rightNodes
  where
    treeNodes = numberOfNodes tree
    lt = fromJust $ leftSubTree tree
    rt = fromJust $ rightSubTree tree
    leftNodes = numberOfNodes lt
    rightNodes = numberOfNodes rt
