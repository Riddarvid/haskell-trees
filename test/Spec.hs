import           BinaryTree.BinaryTree (BinaryTree,
                                        BinaryTreeClass (isEmpty, leftSubTree, numberOfNodes, rightSubTree, traverseTree))
import           BinaryTree.SearchTree (SearchTree,
                                        SearchTreeClass (add, contains, delete))
import           BinaryTree.Traversal  (TraverseOrder (Inorder))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)
import           Test.QuickCheck       (Property, quickCheck, (===), (==>))
main :: IO ()
main = do
  quickCheck propSubTreeSize
  quickCheck propIsOrdered
  quickCheck propContainsAdded
  quickCheck propNotContainsDeleted

type TreeContent = Int

-- Generic binary tree - not ordered.

propSubTreeSize :: BinaryTree TreeContent -> Property
propSubTreeSize tree = (not . isEmpty) tree ==> treeNodes === 1 + leftNodes + rightNodes
  where
    treeNodes = numberOfNodes tree
    lt = fromJust $ leftSubTree tree
    rt = fromJust $ rightSubTree tree
    leftNodes = numberOfNodes lt
    rightNodes = numberOfNodes rt

-- Search tree - ordered, not self balancing.

propIsOrdered :: SearchTree TreeContent -> Property
propIsOrdered = isOrdered . map fst . traverseTree Inorder
  where
    isOrdered nodes = nodes === sort nodes

propContainsAdded :: TreeContent -> SearchTree TreeContent -> Bool
propContainsAdded n tree = contains n (add n tree)

propNotContainsDeleted :: TreeContent -> SearchTree TreeContent -> Bool
propNotContainsDeleted n tree = case delete n tree of
  Nothing         -> notContainsN tree
  Just (_, tree') -> notContainsN tree'
  where
    notContainsN = not . contains n
