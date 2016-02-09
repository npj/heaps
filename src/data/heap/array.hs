module Data.Heap.Array
( Array
) where

import Data.Maybe (fromJust)
import Data.Vector ((!), (!?))
import qualified Data.Vector as V (Vector)

import qualified Data.Heap as H
import qualified Data.Tree as T

newtype Array a = Array { vector :: V.Vector a }

instance T.Empty Array where
  value   = _value
  isEmpty = _isEmpty

instance T.BinaryTree Array where
  left  = _left
  right = _right

instance H.Heap Array where
  empty   = _empty
  isEmpty = _isEmpty
  value   = _value
  insert  = _insert
  remove  = _remove

instance H.BinaryHeap Array

_value :: Array a -> Maybe a
_value = (!? 0) . vector

_isEmpty :: Array a -> Bool
_isEmpty = null . vector

_left :: Array a -> Array a
_left = undefined

_right :: Array a -> Array a
_right = undefined

_empty :: H.Policy -> Array a
_empty = undefined

_insert :: a -> Array a -> Array a
_insert = undefined

_remove :: Array a -> (Maybe a, Array a)
_remove = undefined
