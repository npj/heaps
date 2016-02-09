module Data.Heap
( Policy(Min,Max)
, Heap
, BinaryHeap
, empty
, value
, isEmpty
, insert
, remove
, fromList
, toList
) where

import Data.Maybe (fromJust)

import qualified Data.Tree as T

data Policy = Max | Min deriving (Show)

class Heap h where
  empty     :: Policy -> h a
  isEmpty   :: h a -> Bool
  value     :: h a -> Maybe a
  insert    :: (Ord a) => a -> h a -> h a
  remove    :: (Ord a) => h a -> (Maybe a, h a)

class (T.BinaryTree bh, Heap bh) => BinaryHeap bh

fromList :: (Heap h, Ord a) => Policy -> [a] -> h a
fromList policy = foldr insert (empty policy)

toList :: (Heap h, Ord a) => h a -> [a]
toList heap = if isEmpty heap
                 then []
                 else (fromJust x):(toList rest)
  where (x,rest) = remove heap

