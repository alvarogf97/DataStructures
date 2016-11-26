-------------------------------------------------------------------------------
-- Student's name: <<<write your name here>>>
--
-- Maxiphobic Heaps
-- Data Structures. Grado en Informática. UMA.
-------------------------------------------------------------------------------

module DataStructures.Heap.MaxiphobicHeap
  ( Heap
  , empty
  , isEmpty
  , minElem
  , delMin
  , insert
  , merge
  , mkHeap
  ) where

import Test.QuickCheck


data Heap a  = Empty | Node a Int (Heap a) (Heap a)

-- number of elements in tree rooted at node
weight :: Heap a -> Int
weight Empty           = 0
weight (Node _ w _ _)  = w


singleton :: a -> Heap a
singleton x  = Node x 1 Empty Empty


empty :: Heap a
empty = Empty

isEmpty :: Heap a -> Bool
isEmpty Empty = True
isEmpty _     = False

insert :: (Ord a) => a -> Heap a -> Heap a
insert x h = merge (singleton x) h

node :: a -> Heap a -> Heap a -> Heap a
node x h h'  = Node x s h h'
                where
                  w = weight h
                  w' = weight h'
                  s = w + w' + 1

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge Empty t2 = t2
merge t1 Empty = t1
merge t1@(Node x w lt rt) t2@(Node x' w' lt' rt') | x <= x' = aux1 x lt rt t2
                                                  | otherwise = aux1 x' lt' rt' t1

aux1 :: (Ord a) => a -> Heap a -> Heap a -> Heap a -> Heap a
aux1 x lt rt t2 | max (weight lt) (max (weight rt) (weight t2)) == weight lt = node x (merge rt t2) lt
                | max (weight lt) (max (weight rt) (weight t2)) == weight rt = node x (merge t2 lt) rt
                | max (weight lt) (max (weight rt) (weight t2)) == weight t2 = node x (merge lt rt) t2

minElem :: Heap a -> a
minElem Empty = error"minElem on empty Heap"
minElem (Node x _ _ _) = x

delMin :: (Ord a) => Heap a -> Heap a
delMin Empty            = error"delMin on empty Heap"
delMin (Node _ _ lh rh) = merge lh rh

-- Efficient O(n) bottom-up construction for heaps
mkHeap :: (Ord a) => [a] -> Heap a
mkHeap []  = empty
mkHeap xs  = mergeLoop (map singleton xs)
  where
    mergeLoop [h]  = h
    mergeLoop hs   = mergeLoop (mergePairs hs)

    mergePairs []         = []
    mergePairs [h]        = [h]
    mergePairs (h:h':hs)  = merge h h' : mergePairs hs

-------------------------------------------------------------------------------
-- Generating arbritray Heaps
-------------------------------------------------------------------------------

instance (Ord a, Arbitrary a) => Arbitrary (Heap a) where
  arbitrary  = do
    xs <- arbitrary
    return (mkHeap xs)

instance (Show a) => Show(Heap a) where
  show Empty = ""
  show (Node x w lh rh) = " (" ++ show x ++ "," ++ show w ++ ")" ++ "{" ++ show lh ++ "," ++ show rh ++ "}"
