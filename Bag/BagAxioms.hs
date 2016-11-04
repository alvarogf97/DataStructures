-------------------------------------------------------------------------------
-- Axioms for Bags
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2015
-------------------------------------------------------------------------------

module DataStructures.Bag.BagAxioms( ax1,ax2,ax3,ax4,ax5,ax6,ax7,ax8,ax9,ax10,bagAxioms
                                   , ax11,ax12,ax13,ax14,ax15,ax16,ax17,extBagAxioms
                                   ) where

import DataStructures.Bag.SortedLinearBag
import Test.QuickCheck
import qualified DataStructures.Util.TestDatatype as TDT

-- type Elem = Char -- Test axioms using bags of Ints
type Elem = TDT.Elem -- Test axioms using an enumerated datatype

ax1     = True ==> isEmpty empty
ax2 x b = True ==> not (isEmpty (insert x b))

ax3 x     = True ==> occurrences x empty == 0
ax4 x y b = (x==y) ==> occurrences x (insert y b) == (occurrences x b) + 1
ax5 x y b = (x/=y) ==> occurrences x (insert y b) == occurrences x b

ax6     = True ==> size empty == 0
ax7 x b = True ==> size (insert x b) == 1 + size b

ax8 x      = True ==> delete x empty == empty
ax9 x y b  = x==y ==> delete x (insert y b) == b
ax10 x y b = x/=y ==> delete x (insert y b) == insert y (delete x b)

-- Axioms for extended set of operations
ax11 s        = True ==> union s empty == s
ax12 x b1 b2  = True ==> union b1 (insert x b2) == insert x (union b1 b2)

ax13 b         = True                                 ==> intersection b empty == empty
ax14 x b1 b2   = occurrences x b1 > occurrences x b2  ==> intersection b1 (insert x b2) == insert x (intersection b1 b2)
ax15 x b1 b2   = occurrences x b1 <= occurrences x b2 ==> intersection b1 (insert x b2) == intersection b1 b2
-- alternative definitions for previous axioms:
ax14' x b1 b2   = occurrences x b1 > 0  ==> intersection b1 (insert x b2) == insert x (intersection (delete x b1) b2)
ax15' x b1 b2   = occurrences x b1 == 0 ==> intersection b1 (insert x b2) == intersection b1 b2

ax16 b        = True ==> difference b empty == b
ax17 x b1 b2  = True ==> difference b1 (insert x b2) == delete x (difference b1 b2)

bagAxioms = do
  quickCheck ( ax1 :: Property)
  quickCheck ( ax2 :: Elem -> Bag Elem -> Property)
  quickCheck ( ax3 :: Elem -> Property)
  quickCheck ( ax4 :: Elem -> Elem -> Bag Elem -> Property)
  quickCheck ( ax5 :: Elem -> Elem -> Bag Elem -> Property)
  quickCheck ( ax6 :: Property)
  quickCheck ( ax7 :: Elem -> Bag Elem -> Property)
  quickCheck ( ax8 :: Elem -> Property)
  quickCheck ( ax9 :: Elem -> Elem -> Bag Elem -> Property)
  quickCheck (ax10 :: Elem -> Elem -> Bag Elem -> Property)

-- Includes axioms for extended set of operations
extBagAxioms = do
  bagAxioms
  --quickCheck ( ax11 :: Bag Elem -> Property)
  --quickCheck ( ax12 :: Elem -> Bag Elem -> Bag Elem -> Property)
  --quickCheck ( ax13 :: Bag Elem -> Property)
  --quickCheck ( ax14 :: Elem -> Bag Elem -> Bag Elem -> Property)
  quickCheck ( ax15 :: Elem -> Bag Elem -> Bag Elem -> Property)
  --quickCheck ( ax16 :: Bag Elem -> Property)
  --quickCheck ( ax17 :: Elem -> Bag Elem -> Bag Elem -> Property)
