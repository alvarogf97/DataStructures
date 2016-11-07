module DataStructures.Ring.Ring
  ( Ring
  , empty
  , isEmpty
  , rotL
  , rotR
  , insertL
  , insertR
  , deleteL
  , deleteR
  ) where

import Data.List(intercalate)
import  qualified DataStructures.Stack.LinearStack as S
import Test.QuickCheck

data Ring a = Empty | R (S.Stack a) a (S.Stack a) deriving Show

empty :: Ring a
empty = Empty

isEmpty :: Ring a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- reverse a Stack
invertir :: S.Stack a -> S.Stack a -> S.Stack a
invertir s1 s2 | S.isEmpty s1 = S.empty
               | otherwise = invertir  (S.pop s1) (S.push (S.top s1) s2)

rotL :: (Eq a) => Ring a -> Ring a
rotL Empty = Empty
rotL (R s1 cursor s2) | S.isEmpty s1 = (R (invertir s2 S.empty) (S.top (invertir s2 S.empty)) (S.push cursor S.empty))
                      | otherwise  = (R (S.pop s1) (S.top s1) (S.push cursor s2))

rotR :: (Eq a) => Ring a -> Ring a
rotR Empty = Empty
rotR (R s1 cursor s2) | S.isEmpty s2 = (R (S.push cursor S.empty) (S.top (invertir s1 S.empty)) (invertir s1 S.empty))
                      | otherwise  = (R (S.push cursor s1) (S.top s2) (S.pop s2))

insertL :: (Eq a) => a -> Ring a -> Ring a
insertL a Empty = (R S.empty a S.empty)
insertL a (R s1 cursor s2) = (R (S.push a s1) cursor s2)

insertR :: (Eq a) => a -> Ring a -> Ring a
insertR a Empty = (R S.empty a S.empty)
insertR a (R s1 cursor s2) = (R s1 cursor (S.push a s2))

deleteL :: (Eq a) => Ring a -> Ring a
deleteL (R s1 cursor s2) | S.isEmpty s1 = error "no hay nada a la izquierda"
                         | otherwise = (R (S.pop s1) cursor s2)

deleteR :: (Eq a) => Ring a -> Ring a
deleteR (R s1 cursor s2) | S.isEmpty s2 = error "no hay nada a la derecha"
                         | otherwise = (R s1 cursor (S.pop s2))

--Ring to list
listRing :: Ring a -> [a]
listRing (R s1 cursor s2) = (stackList s1) ++ (cursor : stackList s2)

--Stack to list
stackList :: S.Stack a -> [a]
stackList s1 | S.isEmpty s1 = []
             | otherwise = (S.top s1) : stackList (S.pop s1)

-- Stack equality
instance (Eq a) => Eq (Ring a) where
  Empty            == Empty        =  True
  (R s1 cursor s2) == (R s1' cursor' s2') = listRing (R s1 cursor s2) == listRing (R s1' cursor' s2')
  _                == _            =  False
