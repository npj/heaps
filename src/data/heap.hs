module Data.Heap
( Policy(Min,Max)
, Heap
, BinaryHeap
, policy
, empty
, value
, isEmpty
, insert
, remove
, size
, fromList
, toList
) where

import Data.Maybe (fromJust)

import qualified Data.Tree as T

data Policy = Max | Min deriving (Show)

class Heap h where
  policy    :: h a -> Policy
  empty     :: Policy -> h a
  isEmpty   :: h a -> Bool
  value     :: h a -> Maybe a
  insert    :: (Ord a) => a -> h a -> h a
  remove    :: (Ord a) => h a -> (Maybe a, h a)
  size      :: h a -> Int

  fromList :: (Ord a) => Policy -> [a] -> h a
  fromList policy = foldr insert (empty policy)

  toList :: (Ord a) => h a -> [a]
  toList heap = if isEmpty heap
                   then []
                   else (fromJust x):(toList rest)
    where (x,rest) = remove heap

class (T.BinaryTree bh, Heap bh) => BinaryHeap bh

