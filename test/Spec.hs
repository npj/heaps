import Test.QuickCheck

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Test.Heap as Heap (properties)

tests = Heap.properties

main :: IO ()
main = defaultMain tests

