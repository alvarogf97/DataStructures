-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA
--
-- Titulación: Grado en Ingeniería Software.
-- Alumno: Garcia, Fernández Álvaro
-- Fecha de entrega: DIA | MES | AÑO
--
-- Relación de Ejercicios 1. Ejercicios resueltos: ..........
--
------------------------------------------------------------------------------

-- ejrcicio 1------------------------------------------------------------------
import Test.QuickCheck

esTerna :: Integer -> Integer -> Integer -> Bool
esTerna x y z = if(x*x + y*y == z*z) then True else False

terna :: Integer -> Integer -> (Integer, Integer, Integer)
terna x y = (x*x - y*y, 2*x*y, x*x + y*y)

p_ternas x y = (x>0 && y>0 && x>y) ==> esTerna l1 l2 h
  where
    (l1,l2,h) = terna x y

-- ejercicio 2-----------------------------------------------------------------
intercambia :: (a,b) -> (b,a)
intercambia (a,b) = (b,a)

-- ejercicio 3-----------------------------------------------------------------
ordena2 :: (Ord a) => (a,a) -> (a,a)
ordena2 (x,y) = if(x>y) then (y,x) else (x,y)

p1_ordena2 x y = enOrden (ordena2 (x,y))
  where enOrden (x,y) = x<=y

p2_ordena2 x y = mismosElementos (x,y) (ordena2 (x,y))
  where
    mismosElementos (x,y) (z,v) = (x==z && y==v) || (x==v && y==z)

ordena3 :: (Ord a) => (a,a,a) -> (a,a,a)
ordena3 (x,y,z) = (min x (min y z), min x (max y z),max x (max y z))

p1_ordena3 x y z = enOrden (ordena3 (x,y,z))
  where
   enOrden (x,y,z) = x<=y && y<= z

p2_ordena3 x y z= mismosElementos (x,y,z) (ordena3 (x,y,z))
  where
    mismosElementos (x,y,z) (c,v,b) = (x==c && y==v && z==b)
    || (x==v && y==c && z==b) || (x==b && y==c && z==v )
    || (x==b && y==v && z==c) || (y==b && x==c && z==v) || (y==b && x==v && z==c)

-- ejercicio 4-----------------------------------------------------------------
max2 :: (Ord a) => a -> a -> a
max2 x y = if(x>y) then x else y

p1_max2 x y = coincide (max2 x y)
  where
    coincide x y = x==(max x y) || y==(max x y)

p2_max2 x y = mayor (max2 x y)
  where
    mayor x y = (max2 x y >= x) && (max2 x y >= y)

p3_max2 x y = if max2 x y == x then x>=y else False

p4_max2 x y = if max2 x y == y then y>=x else False

-- ejercicio 5-----------------------------------------------------------------
entre :: Ord a => a -> (a,a) -> Bool
entre x (y,z) = if (x>=y && x<=z) then True else False

-- ejercicio 6-----------------------------------------------------------------
iguales3 :: Eq a => (a,a,a) -> Bool
iguales3 (x,y,z)| (x==y && x==z) = True
                | otherwise = False

-- ejercicio 7-----------------------------------------------------------------
type TotalSegundos = Integer
type Horas = Integer
type Minutos = Integer
type Segundos = Integer

descomponer :: TotalSegundos -> (Horas,Minutos,Segundos)
descomponer x = (horas, minutos, segundos)
    where
          segundos = mod x 60
          horas = div x 3600
          minutos = div x 60 - horas*60

entre :: Ord a => a -> (a,a) -> Bool
entre x (y,z) = if (x>=y && x<=z) then True else False

p_descomponer x = x>=0 ==> h*3600 + m*60 + s == x
                           && entre m (0,59)
                           && entre s (0,59)
    where (h,m,s) = descomponer x

-- ejercicio 8 y 9-------------------------------------------------------------
unEuro :: Double
unEuro = 166.386


pesetasAEuros :: Double -> Double
pesetasAEuros x = x/unEuro

eurosAPesetas :: Double -> Double
eurosAPesetas x = x*unEuro

p_inversas x = eurosAPesetas (pesetasAEuros x) ~= x

infix 4 ~=
(~=) :: Double -> Double -> Bool
x ~= y = abs (x-y) < epsilon
  where epsilon = 1/1000

-- ejercicio 10----------------------------------------------------------------
raices :: Double -> Double -> Double -> (Double, Double)
raices a b c | (b^2 -4*a*c)<0 = error "tienen raices imaginarias"
             | (b^2 -4*a*c)>=0 = (((-b + sqrt(b^2 - 4*a*c))/2*a, (-b - sqrt(b^2 - 4*a*c))/2*a))

p1_raices a b c = esRaíz r1 && esRaíz r2
    where
      (r1,r2) = raices a b c
      esRaíz r = a*r^2 + b*r + c ~= 0

p2_raices a b c = c>0 && (b*b - 4*a*c)>=0 ==> esRaíz r1 && esRaíz r2
    where
        (r1,r2) = raices a b c
        esRaíz r = a*r^2 + b*r + c ~= 0

-- ejercicio 11----------------------------------------------------------------
esMultiplo :: (Integral a) => a -> a-> Bool
esMultiplo x y = if (mod x y == 0) then True else False

-- ejercicio 12----------------------------------------------------------------

infixl 1 ==>>
(==>>) :: Bool -> Bool -> Bool
x ==>> y = if(x == True && y == False) then False else True


-- ejercicio 13----------------------------------------------------------------

esBisiesto :: Integer -> Bool
esBisiesto x | mod x 100 == 0 && mod x 400 == 0 = True
             | mod x 4 == 0 = True
             | otherwise = False

-- ejercicio 14----------------------------------------------------------------

potencia :: Integer -> Integer -> Integer
potencia x y | y == 0 = 1
             | otherwise = x*potencia x (y-1)

potencia2 :: Integer -> Integer -> Integer
potencia2 b n | n == 0 = 1
              | mod n 2 == 0 =  (potencia2 b (div n 2))*(potencia2 b (div n 2))
              | otherwise =  b* (potencia2 b (div (n-1) 2))*(potencia2 b (div (n-1) 2))

p_pot b n = n>=0 ==> potencia b n == sol && potencia2 b n == sol
  where sol = b^n

productos :: Integer -> Integer
productos y | y == 1 = 0
            | otherwise = 1+(productos(y -1));

productos2 :: Integer -> Integer
productos2 y | y == 1 = 0
             | mod y 2 == 0 = 1 + (productos2 (div y 2))
             | otherwise =  2 + (productos2 (div (y-1) 2))

-- ejercicio 15----------------------------------------------------------------

factorial :: Integer -> Integer
factorial x | x == 0 = 1
            | otherwise = x*factorial(x-1)

-- ejercicio 16----------------------------------------------------------------

divideA :: Integer -> Integer -> Bool
divideA x y | x == 0 = False
            | mod y x == 0 = True
            | otherwise = False

p1_divideA x y = y/=0 && y `divideA` x ==> div x y * y == x

p2_divideA x y z = x `divideA` y && x `divideA` z ==> x `divideA` (y+z)

-- ejercicio 17----------------------------------------------------------------

mediana :: (Ord a) => (a,a,a,a,a) -> a
mediana (x,y,z,t,u) | x> z = mediana(z,y,x,t,u)
                    | y> z = mediana(x,z,y,t,u)
                    | t< z = mediana(x,y,t,z,u)
                    | u< z = mediana(x,y,u,t,z)
                    | otherwise = z
