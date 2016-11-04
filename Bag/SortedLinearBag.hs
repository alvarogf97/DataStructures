-------------------------------------------------------------------------------
-- Linear implementation of Bag. Nodes are sorted according to their values
-- and 0 occurences nodes are removed from the structure.
--
-- Student's name: Alvaro García Fenández
-- Student's group:
-------------------------------------------------------------------------------

module DataStructures.Bag.SortedLinearBag
  ( Bag
  , empty
  , isEmpty
  , size
  , insert
  , delete
  , occurrences
  , isElem
  , fold
  , foldOcc
  , union
  , intersection
  , difference
  ) where

import Data.List(intercalate)
import Test.QuickCheck

data Bag a  = Empty | Node a Int (Bag a)  -- the second component stores number of ocurrences/repetitions

empty :: Bag a
empty  = Empty

isEmpty :: Bag a -> Bool
isEmpty Empty  = True
isEmpty _      = False

-- Inserts a new repetition of the element into bag
insert :: (Ord a) => a -> Bag a -> Bag a
insert x Empty  = Node x 1 Empty
insert x (Node y ocurr s)
 | x == y       = Node y (ocurr + 1) s
 | x < y        = Node x 1 (Node y ocurr s)
 | otherwise    = Node y ocurr (insert x s)

-- Returns number of repetitions of element in bag (0 if element is not in bag)
occurrences :: (Ord a) => a -> Bag a -> Int
occurrences x Empty = 0
occurrences x (Node a ocurr b) | x == a = ocurr
                         | otherwise = occurrences x b

-- Removes one repetition of the element from bag
delete :: (Ord a) => a -> Bag a -> Bag a
delete x Empty = Empty
delete x (Node a ocurr b) | (x == a) = if (ocurr - 1 == 0) then b else (Node a (ocurr-1) b)
                          | x /= a = Node a ocurr (delete x b)
-- Return if Bag contains and element x
isElem :: (Ord a) => a -> Bag a -> Bool
isElem x Empty       = False
isElem x (Node y ocurr s)  = x==y || isElem x s

-- Returns total number of elements in bag (takes into account repetitions)
size :: Bag a -> Int
size Empty = 0
size (Node a ocurr b) = ocurr + size b

-- Returns union of two bags (takes into account repetitions: repetitions in result is
-- sum of repetitions in original bags)
union :: (Ord a) => Bag a -> Bag a -> Bag a
union b b' = fold insert b b'

-- Returns intersection of two bags (takes into account repetitions: repetitions in result is
-- minimum of repetitions in original bags)
intersection :: (Ord a) => Bag a -> Bag a -> Bag a
intersection s s' = fold (\x inter -> if isElem x s then insert x inter else inter) empty s'

-- Returns difference of two bags (takes into account repetitions: repetitions in result is
-- difference of repetitions in original bags)
difference :: (Ord a) => Bag a -> Bag a -> Bag a
difference b b' = fold delete b b'



-------------------------------------------------------------------------------
-- Remaining code is already implemented:
-------------------------------------------------------------------------------

fold :: (a -> b -> b) -> b -> Bag a -> b
fold f z  = fun
 where
  fun Empty         = z
  fun (Node x 0 b)  = fun b
  fun (Node x n b)  = f x (fun (Node x (n-1) b))

foldOcc :: (a -> Int -> b -> b) -> b -> Bag a -> b
foldOcc f z  = fun
 where
  fun Empty         = z
  fun (Node x n b)  = f x n (fun b)

-- Showing a bag
instance (Show a) => Show (Bag a) where
        show b  = "SortedLinearBag(" ++ intercalate "," (map show (elems b)) ++")"
          where
            elems Empty         = []
            elems (Node x n b)  = replicate n x ++ elems b

-- Bag equality (assumes nodes are sorted and occurrences > 0)
instance (Eq a) => Eq (Bag a) where
  Empty        == Empty            = True
  (Node x n b) == (Node x' n' b')  = x==x' && n==n' && b==b'
  _            == _                = False

-- Generates a random bag for QuickCheck
instance (Ord a, Arbitrary a) => Arbitrary (Bag a) where
    arbitrary  = do
      xs <- listOf arbitrary
      return (foldr insert empty xs)
