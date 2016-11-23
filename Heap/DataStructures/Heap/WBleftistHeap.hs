module DataStructures.Heap.WBleftistHeap
 ( Heap
 , empty
 , isEmpty
 , minElem
 , delMin
 , insert
 , node
 , singleton
 , weight
 , merge
 )where

data Heap a = Empty | Node a Int (Heap a) (Heap a)

empty :: Heap a
empty = Empty

isEmpty :: Heap a -> Bool
isEmpty Empty = True
isEmpty _     = False

minElem :: Heap a -> a
minElem Empty = error"minElem on empty Heap"
minElem (Node x _ _ _) = x

delMin :: (Ord a) => Heap a -> Heap a
delMin Empty            = error"delMin on empty Heap"
delMin (Node _ _ lh rh) = merge lh rh

singleton :: a -> Heap a
singleton x = Node x 1 Empty Empty

insert :: (Ord a) => a -> Heap a -> Heap a
insert x h = merge (singleton x) h

node :: a -> Heap a -> Heap a -> Heap a
node x h h' | w >= w' = Node x s h h'
            | otherwise = Node x s h' h
              where
                  w = weight h
                  w' = weight h'
                  s = w + w' + 1 -- de la raiz

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge Empty t2 = t2
merge t1 Empty = t1
merge t1@(Node x w lt rt) t2@(Node x' w' lt' rt') | x <= x' = node x lt (merge rt t2)
                                                  | otherwise = node x' lt' (merge t1 rt')

weight :: Heap a -> Int
weight Empty = 0
weight (Node _ w _ _) = w

instance (Show a) => Show(Heap a) where
  show Empty = ""
  show (Node x w lh rh) = " (" ++ show x ++ "," ++ show w ++ ")" ++ "{" ++ show lh ++ "," ++ show rh ++ "}"
