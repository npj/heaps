module Data.Heap.Leftist
( Leftist
) where

import Data.Maybe (fromJust)
import qualified Data.Heap as H
import qualified Data.Tree as T

data Leftist a = Nil H.Policy | Node H.Policy Int a (Leftist a) (Leftist a)

instance T.Empty Leftist where
  value   = _value
  isEmpty = _isEmpty

instance T.BinaryTree Leftist where
  left  = _left
  right = _right

instance H.Heap Leftist where
  empty   = _empty
  isEmpty = _isEmpty
  value   = _value
  insert  = _insert
  remove  = _remove

instance H.BinaryHeap Leftist

_empty :: H.Policy -> Leftist a
_empty p = Nil p

_isEmpty :: Leftist a -> Bool
_isEmpty (Nil _) = True
_isEmpty _       = False

_value :: Leftist a -> Maybe a
_value (Node _ _ r _ _) = Just r
_value _ = Nothing

_left :: Leftist a -> Leftist a
_left (Nil p) = _empty p
_left (Node _ _ _ lt _) = lt

_right :: Leftist a -> Leftist a
_right (Nil p) = _empty p
_right (Node _ _ _ _ rt) = rt

_insert :: (Ord a) => a -> Leftist a -> Leftist a
_insert v heap@(Node p _ _ _ _) = merge heap (singleton p v)

_remove :: (Ord a) => Leftist a -> (Maybe a, Leftist a)
_remove heap = (_value heap, merge (_left heap) (_right heap))

-- private
merge :: (Ord a) => Leftist a -> Leftist a -> Leftist a
merge a (Nil _) = a
merge (Nil _) b = b
merge n1@(Node p rank1 r1 lt1 rt1) n2@(Node _ rank2 r2 lt2 rt2) =
  if hcompare p r1 r2
     then Node p rank1 r1 lt1 (merge rt1 n1)
     else Node p rank2 r2 lt2 (merge rt2 n1)

singleton :: H.Policy -> a -> Leftist a
singleton p v = Node p 0 v (Nil p) (Nil p)

-- private
hcompare :: (Ord a) => H.Policy -> (a -> a -> Bool)
hcompare H.Max= (>)
hcompare H.Min= (<)
