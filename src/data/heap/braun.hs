module Data.Heap.Braun
( Braun
) where

import Data.Maybe (fromJust)
import qualified Data.Heap as H
import qualified Data.Tree as T

data Braun a = Nil H.Policy | Node H.Policy a (Braun a) (Braun a)
  deriving (Show)

instance T.Empty Braun where
  value   = _value
  isEmpty = _isEmpty

instance T.BinaryTree Braun where
  left  = _left
  right = _right

instance H.Heap Braun where
  empty   = _empty
  isEmpty = _isEmpty
  value   = _value
  insert  = _insert
  remove  = _remove

instance H.BinaryHeap Braun

_empty :: H.Policy -> Braun a
_empty p = Nil p

_isEmpty :: Braun a -> Bool
_isEmpty (Nil _) = True
_isEmpty _       = False

_value :: Braun a -> Maybe a
_value (Node _ r _ _) = Just r
_value _ = Nothing

_left :: Braun a -> Braun a
_left (Nil p) = _empty p
_left (Node _ _ lt _) = lt

_right :: Braun a -> Braun a
_right (Nil p) = _empty p
_right (Node _ _ _ rt) = rt

-- inserting into a Braun tree works as follows:
-- * inserting a value into a nil tree creates a new node
-- * otherwise, if the value passes the comparison (either gt or lt depending on
--   the tree type), return a new node where the left tree is the result of
--   inserting the value into the right subtree. this works out to "insert
--   into the right subtree and swap the subtrees."
_insert :: (Ord a) => a -> Braun a -> Braun a
_insert v (Nil p) = singleton p v
_insert v (Node p r lt rt) =
  if hcompare p v r
     then Node p v (_insert r rt) lt
     else Node p r (_insert v rt) lt

-- removing involves taking the last item in the tree and replacing the root
-- value with that value
_remove :: (Ord a) => Braun a -> (Maybe a, Braun a)
_remove (Nil p) = (Nothing, _empty p)
_remove node@(Node p v lt rt)
  | _isEmpty lt = (Just v, _empty p)
  | _isEmpty rt = (Just v, lt)
  | otherwise       = (Just v, replace last rest)
  where (last, rest) = takeLast node

-- private
singleton :: H.Policy -> a -> Braun a
singleton p v = Node p v (Nil p) (Nil p)

-- private
takeLast :: (Ord a) => Braun a -> (a, Braun a)
takeLast (Node p r (Nil _) (Nil _)) = (r, _empty p)
takeLast (Node p r lt (Nil _))    = (l, singleton p r)
  where l = (fromJust . _value $ lt)
takeLast (Node p r lt rt) = (last, Node p r rt rest)
  where (last, rest) = takeLast lt

-- private
replace :: (Ord a) => a -> Braun a -> Braun a
replace val (Nil p) = singleton p val
replace val (Node p _ (Nil _ ) (Nil _)) = singleton p val
replace v (Node p _ lt rt) | replaceRoot = Node p v lt rt
                           | replaceLeft = Node p l (replace v lt) rt
                           | otherwise   = Node p r lt (replace v rt)
  where replaceRoot
          | _isEmpty rt && hcompare p v l = True
          | hcompare p v l  && hcompare p v r = True
          | otherwise                         = False

        replaceLeft
          | _isEmpty rt && hcompare p l v = True
          | hcompare p l r                    = True
          | otherwise                         = False

        l = (fromJust . _value $ lt)
        r = (fromJust . _value $ rt)

-- private
hcompare :: (Ord a) => H.Policy -> (a -> a -> Bool)
hcompare H.Max = (>=)
hcompare H.Min = (<=)

