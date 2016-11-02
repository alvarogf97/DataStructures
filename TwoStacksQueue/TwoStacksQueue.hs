

module DataStructures.Queue.TwoStacksQueue
  ( Queue
  , empty
  , isEmpty
  , enqueue
  , first
  , dequeue
  ) where

import Data.List(intercalate)
import Test.QuickCheck
import qualified DataStructures.Stack.LinearStack as S

data Queue a = Q (S.Stack a) (S.Stack a)

empty :: Queue a
empty = Q S.empty S.empty

isEmpty :: Queue a -> Bool
isEmpty (Q s1 s2) = S.isEmpty s1 && S.isEmpty s2

enqueue :: a -> Queue a -> Queue a
enqueue x (Q s1 s2) = (Q (S.push x s1) s2)

dequeue :: Queue a -> Queue a -- volcamos la inbox en la outbox eliminamos el elemento y volvemos a volcar
dequeue (Q inbox outbox) | isEmpty (Q inbox outbox) = error "la cola esta vacia"
                         | otherwise = Q (invertir (S.pop (invertir inbox outbox)) S.empty) outbox

invertir :: S.Stack a -> S.Stack a -> S.Stack a
invertir s1 s2 | S.isEmpty s1 = s2
               | otherwise = invertir (S.pop s1) (S.push (S.top s1) s2)


first :: Queue a -> a
first (Q s1 s2) | isEmpty (Q s1 s2) = error"no hay elementos"
                | otherwise = S.top (invertir s1 s2)




-- Showing a queue
instance (Show a) => Show (Queue a) where
  show q = "TwoStacksQueue(" ++ intercalate "," (aux q) ++ ")"
    where
        aux q
          | isEmpty q = []
          | otherwise = show (first q) : aux (dequeue q)

-- Queues equality
instance (Eq a) => Eq (Queue a) where
  q1 == q2
    | e1 && e2         = True
    | not e1 && not e2 = first q1 == first q2 && dequeue q1 == dequeue q2
    | otherwise        = False
    where
      e1 = isEmpty q1
      e2 = isEmpty q2

-- This instance is used by QuickCheck to generate random queues
instance (Arbitrary a) => Arbitrary (Queue a) where
    arbitrary = do
      xs <- listOf arbitrary
      return (foldr enqueue empty xs)
