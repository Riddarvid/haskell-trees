{-# LANGUAGE InstanceSigs #-}

module BinaryTree.Internal.BinaryTree (
  BinaryTree(..)
) where

data BinaryTree a = BNode a (BinaryTree a) (BinaryTree a) | BEmpty

instance Functor BinaryTree where
  fmap :: (a -> b) -> BinaryTree a -> BinaryTree b
  fmap _ BEmpty          = BEmpty
  fmap f (BNode e n1 n2) = BNode (f e) (fmap f n1) (fmap f n2)
