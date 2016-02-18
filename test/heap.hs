{-# LANGUAGE TemplateHaskell #-}

module Heap
( properties
) where

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck (testProperty, QuickCheckTests(..))

import Test.QuickCheck ((==>))
import Test.QuickCheck.Modifiers (NonEmptyList(NonEmpty))

import Language.Haskell.Extract (functionExtractorMap)

import Data.Maybe (isJust, isNothing)
import Data.List (sort)

import qualified Data.Heap as H
import qualified Data.Heap.Braun as B
import qualified Data.Heap.Leftist as L

type FromList h = ([Int] -> h Int)

data Builder =   Braun   (FromList B.Braun)
               | Leftist (FromList L.Leftist)

builders :: [(String, Builder)]
builders = [
    ("Braun Min Heap", Braun(H.fromList H.Min))
  , ("Braun Max Heap", Braun(H.fromList H.Max))
  , ("Leftist Min Heap", Leftist(H.fromList H.Min))
  , ("Leftist Max Heap", Leftist(H.fromList H.Max))
  ]

heapPropertiesFor :: Builder -> [TestTree]
heapPropertiesFor (Braun f)   = map ($ f) heapProperties
heapPropertiesFor (Leftist f) = map ($ f) heapProperties

-- finds all functions with a "prop_" prefix and turns them into Tasty tests
-- which expect a contrete H.fromList implementation
heapProperties :: (H.Heap h) => [FromList h -> TestTree]
heapProperties = $(functionExtractorMap "^prop_" [| \l f -> testProperty l . f |])

withOptions :: TestTree -> TestTree
withOptions = localOption (QuickCheckTests 1000)

properties :: TestTree
properties = withOptions $ testGroup "Heap Properties" . map group $ builders
  where group (label, builder) = testGroup label $ heapPropertiesFor builder

-- an empty heap has Nothing as its value
prop_noValue :: (H.Heap h) => FromList h -> Bool
prop_noValue fromList = isNothing . H.value . fromList $ []

-- a non-empty heap has something as its value
prop_value :: (H.Heap h) => FromList h -> NonEmptyList Int -> Bool
prop_value fromList (NonEmpty xs) = isJust . H.value . fromList $ xs

-- an empty heap is empty
prop_isEmpty :: (H.Heap h) => FromList h -> Bool
prop_isEmpty fromList = H.isEmpty . fromList $ []

-- a heap is non-empty after inserting a value
prop_nonEmptyAfterInsert :: (H.Heap h) => FromList h -> Int -> Bool
prop_nonEmptyAfterInsert fromList x = not . H.isEmpty . H.insert x . fromList $ []

-- a heap is empty after remove the only value
prop_emptyAfterRemove :: (H.Heap h) => FromList h -> Int -> Bool
prop_emptyAfterRemove fromList x =
  H.isEmpty . snd . H.remove . H.insert x . fromList $ []

-- a heap has one more element after inserting
prop_sizeAfterInsert :: (H.Heap h) => FromList h -> Int -> [Int] -> Bool
prop_sizeAfterInsert fromList x xs =
  let heap = fromList xs in (H.size . H.insert x $ heap) == (H.size heap) + 1

-- a heap has one less element after removing
prop_sizeAfterRemove :: (H.Heap h) => FromList h -> NonEmptyList Int -> Bool
prop_sizeAfterRemove fromList (NonEmpty xs) =
  let heap = fromList xs in (H.size . snd . H.remove $ heap) == (H.size heap) - 1

-- the root of a min heap is the (min/max) element in the set
prop_root :: (H.Heap h) => FromList h -> NonEmptyList Int -> Bool
prop_root fromList (NonEmpty xs) = (H.value heap) == (Just . minmax $ xs)
    where heap = fromList xs
          minmax = case H.policy heap of
                     H.Min -> minimum
                     H.Max -> maximum

-- converting a heap from a list then back to a list sorts it
prop_sort :: (H.Heap h) => FromList h -> [Int] -> Bool
prop_sort fromList xs = (H.toList heap) == msort xs
  where heap = fromList xs
        msort = case H.policy heap of
                  H.Min -> sort
                  H.Max -> reverse . sort
