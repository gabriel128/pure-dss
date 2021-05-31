{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SetImpls where

import Set
import Bst

type UnbalancedTreeSet = Tree

instance (Ord a) => Set (UnbalancedTreeSet a) a where
  empty :: Ord a => UnbalancedTreeSet a
  empty = Leaf

  -- | Inserts a into the tree
  --
  -- Examples: (Should fail)
  --
  -- >>> Set.insert 3 . Set.insert 5 . Set.insert 4 $ Set.new :: UnbalancedTreeSet Int
  -- [2]
  insert :: Ord a => a -> UnbalancedTreeSet a -> UnbalancedTreeSet a
  insert x Leaf = Node Leaf x Leaf
  insert x node@(Node ltree y rtree)
    | x < y = Node (insert x ltree) y rtree
    | x > y = Node ltree y (insert x rtree)
    | x == y = node

  member :: Ord a => a -> UnbalancedTreeSet a -> Bool
  member x Leaf = False
  member x node@(Node ltree y rtree)
    | x < y = member x ltree
    | x > y = member x rtree
    | x == y = True

