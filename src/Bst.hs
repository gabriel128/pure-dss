{-# LANGUAGE InstanceSigs #-}
module Bst where

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Eq, Show, Ord)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf = Leaf
  fmap f (Node ltree x rtree) = Node (fmap f ltree) (f x) (fmap f rtree)

inOrderT :: Tree a -> [a]
inOrderT Leaf = []
inOrderT (Node ltree x rtree) = inOrderT ltree ++ [x] ++ inOrderT rtree
