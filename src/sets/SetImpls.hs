{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SetImpls where

import Set
import Bst

type UnbalancedSet = Tree

instance (Ord a) => Set (UnbalancedSet a) a where
  empty :: Ord a => UnbalancedSet a
  empty = Leaf

  insert :: Ord a => a -> UnbalancedSet a -> UnbalancedSet a
  insert x Leaf = Node Leaf x Leaf
  insert x node@(Node ltree y rtree)
    | x < y = Node (insert x ltree) y rtree
    | x > y = Node ltree y (insert x rtree)
    | x == y = node

  member :: Ord a => a -> UnbalancedSet a -> Bool
  member x Leaf = False
  member x node@(Node ltree y rtree)
    | x < y = member x ltree
    | x > y = member x rtree
    | x == y = True
