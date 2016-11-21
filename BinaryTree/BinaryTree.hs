
data TreeB a = EmptyB | NodeB a (TreeB a) (TreeB a) deriving Show

tree :: TreeB Int
tree = NodeB 1 (NodeB 2 (NodeB 4 EmptyB EmptyB) (NodeB 5 EmptyB EmptyB))
               (NodeB 3 (NodeB 6 EmptyB EmptyB) EmptyB)

sumB :: (Num a) => TreeB a -> a
sumB EmptyB = 0
sumB (NodeB x lt rt) = x + sumB lt + sumB rt

atLevelB :: Int -> TreeB a -> [a]
atLevelB _ EmptyB = []
atLevelB 0 (NodeB x lt rt) = [x]
atLevelB n (NodeB x lt rt) = atLevelB (n-1) lt ++ atLevelB (n-1) rt

pathsToB :: (Eq a) => a -> TreeB a -> [[a]]
pathsToB x EmptyB = []
pathsToB x (NodeB y lt rt) | x == y = [y] : ps
                           | otherwise = ps
                          where
                              ps = map (y:) (pathsToB x lt ++ pathsToB x rt)

insert :: (Ord a) => a -> TreeB a -> TreeB a
insert m EmptyB = (NodeB m EmptyB EmptyB)
insert m (NodeB x lt rt) | complete (NodeB x lt rt) == True = (NodeB x (insert m lt) rt)
                         | otherwise = (NodeB x lt (insert m rt))

altura :: TreeB a -> Int
altura EmptyB = 0
altura (NodeB y lt rs) = 1 + max (altura lt) (altura rs)

complete :: TreeB a -> Bool
complete EmptyB = True
complete tr = length (atLevelB ((altura tr)-1) tr) == 2^((altura tr)-1)
