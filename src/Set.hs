{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Set where

class Set s a | s -> a where
  empty :: s
  insert :: a -> s -> s
  member :: a -> s  -> Bool

  new :: s
  new = empty
