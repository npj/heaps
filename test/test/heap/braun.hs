{-# LANGUAGE TemplateHaskell #-}

module Test.Heap.Braun
( props
) where

import qualified Data.Heap as H
import qualified Data.Tree as T
import Data.Heap.Braun (Braun)

import Test.QuickCheck.All
import Test.QuickCheck ((==>))

-- properties of Empty typeclass
-- an empty thing has Nothing as its value
-- a non-empty thing has something as its value
prop_noValueMin = H.value (H.empty H.Min :: Braun Int) == Nothing

prop_noValueMax = H.value (H.empty H.Max :: Braun Int) == Nothing

-- a heap is non empty after inserting a value
prop_nonEmptyMin x xs =
  H.isEmpty (H.fromList H.Min xs :: Braun Int) ==>
  (not . H.isEmpty . H.insert x $ (H.fromList H.Min xs :: Braun Int))

return []
props = $quickCheckAll
