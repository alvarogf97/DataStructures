module Main where

import DataStructures.Heap.WBleftistHeap
import Test.QuickCheck

main :: IO()
main = do
  print b1
  print b2

b1 :: Heap Int
b1 = insert 5 (insert 3 (insert 2 (insert 0 empty)))

b2 :: Heap Int
b2 = insert 1 (insert 6 (insert 0 empty))
