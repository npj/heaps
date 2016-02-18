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
  policy  = _policy
  empty   = _empty
  isEmpty = _isEmpty
  value   = _value
  insert  = _insert
  remove  = _remove
  size    = T.size

instance H.BinaryHeap Leftist

_policy :: Leftist a -> H.Policy
_policy (Nil p) = p
_policy (Node p _ _ _ _) = p

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
_insert v (Nil p) = singleton p v
_insert v heap@(Node p _ _ _ _) = merge heap (singleton p v)

_remove :: (Ord a) => Leftist a -> (Maybe a, Leftist a)
_remove (Nil p) = (Nothing, _empty p)
_remove heap    = (_value heap, merge (_left heap) (_right heap))

-- private
merge :: (Ord a) => Leftist a -> Leftist a -> Leftist a
merge a (Nil _) = a
merge (Nil _) b = b
merge n1@(Node p _ r1 lt1 rt1) n2@(Node _ _ r2 _ _) =
  if hcompare p r2 r1
     then merge n2 n1
     else let merged    = merge rt1 n2
              leftDist  = dist lt1
              rightDist = dist merged
           in if rightDist > leftDist
                 then Node p (leftDist  + 1) r1 merged lt1
                 else Node p (rightDist + 1) r1 lt1 merged

-- private
dist :: Leftist a -> Int
dist (Nil _)          = -1
dist (Node _ d _ _ _) = d

singleton :: H.Policy -> a -> Leftist a
singleton p v = Node p 0 v (Nil p) (Nil p)

-- private
hcompare :: (Ord a) => H.Policy -> (a -> a -> Bool)
hcompare H.Max = (>)
hcompare H.Min = (<)
