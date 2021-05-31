{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Stack where

import Prelude hiding (head, tail)

class Stack stack where
  empty :: stack a
  isEmpty :: stack a -> Bool
  cons :: a -> stack a -> stack a
  head :: stack a -> (Maybe a, stack a)
  tail :: stack a -> stack a

  new :: stack a
  new = empty

  -- Pop(ular) interface
  pop :: stack a -> (Maybe a, stack a)
  pop = head

  push :: a -> stack a -> stack a
  push = cons


newtype ListStack a = ListStack [a]
  deriving (Eq, Show)

instance Stack ListStack where
  empty :: ListStack a
  empty  = ListStack []

  isEmpty :: ListStack a -> Bool
  isEmpty (ListStack []) = True
  isEmpty _ = False

  cons :: a -> ListStack a -> ListStack a
  cons x (ListStack xs) = ListStack (x : xs)

  head :: ListStack a -> (Maybe a, ListStack a)
  head (ListStack []) = (Nothing, ListStack [])
  head (ListStack (x:xs)) = (Just x, ListStack xs)


  tail :: ListStack a -> ListStack a
  tail (ListStack []) = ListStack []
  tail (ListStack [x]) = ListStack [x]
  tail (ListStack (x:xs)) = ListStack xs
