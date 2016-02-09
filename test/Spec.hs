import Test.QuickCheck

import qualified Test.Heap.Braun as Braun (props)

main :: IO ()
main = Braun.props >> return ()
