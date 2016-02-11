module Data.Tree
( Empty
, BinaryTree
, value
, isEmpty
, left
, right
, size
, prettify
) where

import Data.Maybe (fromJust)

-- things that can be empty
class Empty t where
  value     :: t a -> Maybe a
  isEmpty   :: t a -> Bool

class (Empty t) => BinaryTree t where
  left  :: t a -> t a
  right :: t a -> t a

  size  :: t a -> Int
  size t = if isEmpty t
              then 0
              else (size . left $ t) + (size . right $ t) + 1

prettify :: (BinaryTree t, Show a) => t a -> String
prettify = prettify' 0
  where prettify' indent tree = if indent == 0
                                   then toString
                                   else "\n" ++ format
          where format   = spaces  ++ "|-" ++ toString
                spaces   = (replicate (indent - 2) ' ')
                toString = (show . fromJust . value $ tree) ++
                           (prettify' (indent + 2) (left tree)) ++
                           (prettify' (indent + 2) (right tree))

