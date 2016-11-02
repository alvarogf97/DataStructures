-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- (completa y sustituye los siguientes datos)
-- Titulación: Grado en Ingeniería del Software.
-- Alumno: García Fernández Álvaro
-- Fecha de entrega: DIA | MES | AÑO
--
-- Relación de Ejercicios 2. Ejercicios resueltos: ..........
--
-------------------------------------------------------------------------------
import Test.QuickCheck
import qualified DataStructures.Stack.LinearStack as S
import qualified DataStructures.Queue.LinearQueue as Q

-- ejercicio 1-----------------------------------------------------------------

data Direction = North | South | East | West
  deriving (Eq,Enum,Show)

instance Ord Direction where
  North <= _ = North << South
  South <= North = False
  South <= _ = True
  East <= North = False
  East <= South = False
  East <= _ = True
  West <= West = True
  West <= _ = False

(<<) :: Direction -> Direction -> Bool
(<<) x y | x == North && (y /= North)= True
         | x == South && (y == East || y == West) = True
         | x == East && y == West = True
         | otherwise = False

p_menor x y = (x < y) == (x << y)

instance Arbitrary Direction where
          arbitrary = do
                  n <- choose (0,3)
                  return $ toEnum n

-- ejercicio 2-----------------------------------------------------------------

maximoYrestoa :: (Ord a) => [a] -> (a,[a])
maximoYrestoa (x:xs) = (maximum (x:xs), (filter(\y -> y < maximum(x:xs)) (x:xs)))

-- ejercicio 3-----------------------------------------------------------------

reparte :: [a] -> ([a],[a])
reparte [] = ([],[])
reparte (x: xs) | length xs == 0 =  (x : [], [])
                | length xs == 1 = (x : [], xs)
                | otherwise = (x : fst(reparte(drop 1 xs)), head xs : snd(reparte(drop 1 xs)))


-- ejercicio 4-----------------------------------------------------------------

distintos :: (Ord a) => [a] -> Bool
distintos [] = True
distintos (x:xs) | length xs == 0 = True
                 | otherwise = ((aux x xs) && distintos xs)

aux :: (Ord a) => a -> [a] -> Bool
aux n [] = True
aux n (x:xs) =  ((n /= x) && (aux n xs))

-- ejercicio 5-----------------------------------------------------------------

replicate' :: Int -> a -> [a]
replicate' n x | n == 0 = []
               | otherwise = [x | x <- [x]] ++ replicate' (n-1) x

p_replicate' n x = n >= 0 && n <= 1000 ==>
                              length (filter (==x) xs) == n
                              && length (filter (/=x) xs) == 0
                                where xs = replicate' n x

-- ejercicio 6-----------------------------------------------------------------

divideA :: Integer -> Integer -> Bool
divideA x y | x == 0 = False
            | mod y x == 0 = True
            | otherwise = False

divideA' :: Int -> Int -> Bool
divideA' x y | x == 0 = False
            | mod y x == 0 = True
            | otherwise = False

divisores :: Integer -> [Integer]
divisores n | n == 0 = []
            | otherwise = [x | x <- [1..n], divideA x n]

divisores' :: Int -> [Int]
divisores' n | n == 0 = []
             | n < 0 = [x | x <- [(n)..(-1)], divideA' x n] ++ [x | x <- [1..(-n)], divideA' x n]
             | otherwise = [x | x <- [(-n)..(-1)], divideA' x n] ++ [x | x <- [1..n], divideA' x n]

-- ejercicio 7-----------------------------------------------------------------

mcd :: Integer -> Integer -> Integer
mcd x y | (x==0 && y ==0) = error "todo numero natural es divisor comun"
        | x == 0 = y
        | y == 0 = x
        | otherwise = maximum ([n | n <- [1..(min x y)], (divideA n x) && (divideA n y)])

p_mcd x y z = (x>0)&&(y>0)&&(z>0) ==> (mcd (z*x) (z*y)) == z*(mcd x y)

mcm :: Integer -> Integer -> Integer
mcm x y = if(x/=0 && y/=0) then div (x*y) (mcd x y) else error "todo numero natural es divisor comun"

-- ejercicio 8-----------------------------------------------------------------

esPrimo :: Integer -> Bool
esPrimo x | x == 0 = False
          | x == 1 = True
          | otherwise = (length(divisores x) == 2)

primosHasta :: Integer -> [Integer]
primosHasta x | x == 0 = []
              | otherwise = [n | n <- [1..x], esPrimo n]

primosHasta' :: Integer -> [Integer]
primosHasta' x | x == 0 = []
              | otherwise = filter (\x -> esPrimo x == True) [1..x]

p1_primos x = primosHasta x == primosHasta' x

-- ejercicio 9-----------------------------------------------------------------


pares :: Integer -> [(Integer,Integer)]
pares x | x == 0 = []
        | otherwise = [(p1,p2) | p1 <- primosHasta x, p2 <- primosHasta x, p1 + p2 == x, p1 > 1 && p2>1]

goldbach :: Integer -> Bool
goldbach x | length (pares x) == 0 = False
           | otherwise = True

goldbachHasta :: Integer -> Bool
goldbachHasta n | n <2 = error "Debe se >2"
                | otherwise = and [goldbach x | x <- [3..n], even x]

-- ejercicio 10----------------------------------------------------------------

esPerfecto :: Integer -> Bool
esPerfecto x | x == (foldr (+) 0 (divisores x)) - x = True
             | otherwise = False;

perfectosMenoresQue :: Integer -> [Integer]
perfectosMenoresQue x | x == 0 = []
                     | otherwise = [x | x <- [1..x], esPerfecto x]

-- ejercicio 11----------------------------------------------------------------

take' :: Int -> [a] -> [a]
take' n xs = [ x | (p,x) <- zip [0..(n-1)] xs]

drop' :: Int -> [a] -> [a]
drop' n xs = [x | (p,x) <- zip [0..(length xs)] xs, p>=n]

p_ej11 n xs = (n>=0) ==> (take' n xs) ++ (drop' n xs) == xs

-- ejercicio 12----------------------------------------------------------------

concat' :: [[a]] -> [a]
concat' [] = []
concat' x = foldr (++)[] x

concat'' :: [[a]] -> [a]
concat'' [] = []
concat'' (x:xs) = x ++ concat'' xs -- no es asi

-- ejercicio 13----------------------------------------------------------------

desconocida :: (Ord a) => [a] -> Bool
desconocida xs = and [ x<=y | (x,y) <- zip xs (tail xs) ]
-- te dice si esta ordenado de menor a mayor

-- ejercicio 14----------------------------------------------------------------

inserta :: (Ord a) => a -> [a] -> [a]
inserta n x | x == [] = n : x
            | otherwise = (takeWhile (<n) x)++ (n : []) ++ (dropWhile (<n) x)

inserta' :: (Ord a) => a -> [a] -> [a]
inserta' n [] = n : []
inserta' n (x:xs) | n <= x = n : x : xs
                  | otherwise = x : (inserta' n xs)

p1_inserta x xs = desconocida xs ==> desconocida (inserta x xs)

ordena :: (Ord a) => [a] -> [a]
ordena [] = []
ordena x = foldr (inserta)[] x

-- ejercicio 15----------------------------------------------------------------

geometrica :: Integer -> Integer -> [Integer]
geometrica n r | r == 0 = error "r debe ser mayor que 0"
               | otherwise = iterate (*r) n

p1_geometrica x r = x>0 && r>0 ==> and [ div z y == r | (y,z) <- zip xs (tail xs) ]
  where xs = take 100 (geometrica x r)

multiplosDe :: Integer -> [Integer]
multiplosDe x | x == 0 = error "x debe ser mayor que 0"
              | otherwise = iterate (+x) 0

potenciasDe :: Integer -> [Integer]
potenciasDe x | x == 0 = error "x debe ser mayor que 0"
              | otherwise = iterate (*x) 0

-- ejercicio 16----------------------------------------------------------------
multiplosDe' :: Integer -> [Integer]
multiplosDe' x  = iterate (+x) x

primeroComun :: (Ord a) => [a] -> [a] -> a
primeroComun (x:xs) (y:ys) | x == y = x
                           | x < y = primeroComun xs (y:ys)
                           | otherwise = primeroComun (x:xs) ys

mcm' :: Integer -> Integer -> Integer
mcm' x y = primeroComun (multiplosDe' x) (multiplosDe' y)

p_mcm' x y = x>0 && y>0 ==> mcm' x y == lcm x y

-- ejercicio 17----------------------------------------------------------------

primeroComunDeTres :: (Ord a) => [a] -> [a] -> [a] -> a
primeroComunDeTres xs ys zs = head [x | x<-xs, y<-ys, z<- zs, (x == y)&&(x==z)]

-- ejercicio 18----------------------------------------------------------------

factPrimos :: Integer -> [Integer]
factPrimos x = fp x 2
  where
    fp x d
      | (esPrimo d) == False = fp x (d+1)
      | x' < d = [x]
      | r == 0 = d : fp x' d
      | otherwise = fp x (d+1)
        where (x',r) = divMod x d -- cociente y resto

p_factPrimos x = x>0 ==> product (factPrimos x) == x

-- ejercicio 19----------------------------------------------------------------

mezcla :: [Integer] -> [Integer] -> [Integer]
mezcla (x:xs) (y:ys) = aux 0 (x:xs) (y:ys)
  where
    aux n [] [] = []
    aux n [] (y:ys) = y : aux n [] ys
    aux n (x:xs) [] = x : aux n xs []
    aux n (x:xs) (y:ys) | x == y = x : aux x xs ys
                        | x /= y && x == n = x : aux n xs (y:ys)
                        | x /= y && y == n = y : aux n (x:xs) ys
                        | x /= y = aux x (x:xs) (y:ys)

mcm'' :: Integer -> Integer -> Integer
mcm'' x y = product (mezcla (factPrimos x) (factPrimos y))

p_mcm'' x y = x>=0 && y>=0 ==> mcm'' x y == lcm x y
-- ejercicio 20----------------------------------------------------------------

mezc2 :: [Integer] -> [Integer] -> [Integer]
mezc2 (x:xs) (y:ys) = aux2 0 (x:xs) (y:ys)
  where
    aux2 n [] [] = []
    aux2 n (x:xs) [] = []
    aux2 n [] (y:ys) = []
    aux2 n (x:xs) (y:ys) | x == y = x : aux2 x xs ys
                         | x /= y && n == x = aux2 x [x | x <- xs, x /=n] (y:ys)
                         | x /= y && x > y = aux2 x (x:xs) [m | m <- ys, m /= y]
                         | x /= y && y > x = aux2 x [m | m <- xs, m /= x] (y:ys)
                         | otherwise =  aux2 x xs ys

mcd'' :: Integer -> Integer -> Integer
mcd'' x y = product (mezc2 (factPrimos x) (factPrimos y))

p_mcd'' x y = x>0 && y>0 ==> mcd'' x y == gcd x y

-- ejercicio 21----------------------------------------------------------------

p_neutroDer xs ys = xs == [] && ys /= [] ==> (xs ++ ys) == (ys ++ xs) && (xs ++ ys) == ys

-- ejercicio 22----------------------------------------------------------------

p_asociativa xs ys zs = (xs ++ ys) ++ zs == xs ++ (ys ++ zs)

-- ni idea loko

-- ejercicio 23----------------------------------------------------------------

nub' :: (Ord a) => [a] -> [a]
nub' [] = []
nub' (x:xs) = x : nub' [m | m <- xs, m /= x]

-- p_nub' xs = nub xs == nub' xs biblioteca List

todosEn :: (Eq a) => [a] -> [a] -> Bool
ys `todosEn` xs = all (`elem` xs) ys

p_sinRepes' xs = todosEn (nub' xs) xs

-- ejercicio 24----------------------------------------------------------------

binarios :: Integer -> [[Char]]
binarios x | x == 0 = [""]
           | otherwise = (aux3 "0" (binarios (x-1))) ++ (aux3 "1" (binarios (x-1)))

aux3 :: [Char] -> [[Char]] -> [[Char]]
aux3 xs yss = [xs ++ p | p <- yss]

long :: [a] -> Integer
long xs = fromIntegral (length xs)

p_binarios n = n>=0 && n<=10 ==>
                    long xss == 2^n
                    && distintos xss
                    && all (`todosEn` "01") xss
                where xss = binarios n

-- ejercicio 25----------------------------------------------------------------
varRep :: Integer -> [Char] -> [[Char]]
varRep x ys | x == 0 = [""]
            | x == 1 = [[m] | m <- ys]
            | otherwise = concat [aux5 [m2] (varRep (x-1) ys) | m2 <- [[m3] | m3 <- ys]]


aux5 :: [[Char]] -> [[Char]] -> [[Char]]
aux5 [""] (y:yss) = [""]
aux5 xss yss = [xs ++ p | xs <- xss, p <- yss]

p_varRep m xs = m >=0 && m <= 5 && n <= 5 && distintos xs ==>
                                          long vss == n^m
                                          && distintos vss
                                          && all (`todosEn` xs) vss
     where vss = varRep m xs
           n = long xs

cardinal :: Integer -> [Char] -> Int
cardinal x ys = length (varRep x ys)

-- ejercicio 26----------------------------------------------------------------

varRep' :: Integer -> [Char] -> [[Char]]
varRep' x ys | x == 0 = [""]
            | x == 1 = [[m] | m <- ys]
            | otherwise = concat [aux6 [m2] (varRep' (x-1) ys) | m2 <- [[m3] | m3 <- ys]]


aux6 :: [[Char]] -> [[Char]] -> [[Char]]
aux6 [""] (y:yss) = [""]
aux6 xss yss = [xs ++ p | xs <- xss, p <- yss, not (aux7 xs p)]

aux7 :: [Char] -> [Char] -> Bool
aux7 [] ys = True
aux7 xs [] = False
aux7 (x:xs) (y:ys) | x == y = aux7 xs ys
                   | x /= y = aux7 (x:xs) ys


p_var m xs = n<=5 && distintos xs && m>=0 && m<=n ==>
                    long vss == fact n `div` fact (n-m)
                    && distintos vss
                    && all distintos vss
                    && all (`todosEn` xs) vss
              where
                vss = varRep' m xs
                n = long xs
                fact :: Integer -> Integer
                fact x = product [1..x]

cardinal2 :: Integer -> [Char] -> Int
cardinal2 n xs = length (varRep' n xs)

-- ejercicio 27----------------------------------------------------------------
intercala :: a -> [a] -> [[a]]
intercala x [] = [[x]]
intercala x (y:ys) = (x:y:ys) : [y:zs | zs <- intercala x ys]

permutaciones :: [a] -> [[a]]
permutaciones []     = [[]]
permutaciones (x:xs) = concat [intercala x ys | ys <- permutaciones xs]

-- ejemplo---------------------------------------------------------------------

ejemplo :: S.Stack a -> a
ejemplo x = S.top x

ejemplo2 :: Q.Queue a -> a
ejemplo2 x = Q.first x

crearcola :: [a] -> Q.Queue a
crearcola [] = Q.empty
crearcola (x:xs) = Q.enqueue x (crearcola xs)
