
absoluto :: (Ord a, Num a) => a -> a
absoluto k | k >=0 = k
           | k<0 = -k

absolutoR :: Float -> Float
absolutoR x | x >= 0 = x
            | x<0 = -x


maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto a b | absoluto(a) >= absoluto(b) = absoluto(a)
                   | otherwise = absoluto(b)


maximo3 :: Integer -> Integer -> Integer -> Integer 
maximo3 a b c | maximo a b >= c = maximo a b
              | otherwise = c
              

maximo :: Integer -> Integer -> Integer
maximo x y | x>=y = x 
           | x<y  = y

algunoEs0 :: Integer -> Integer -> Bool
algunoEs0 x y = (x==0 || y==0)

algunoEs0PM :: Integer -> Integer -> Bool
algunoEs0PM 0 _ = True
algunoEs0PM _ 0 = True
algunoEs0PM _ _ = False 

ambosSon0 :: Integer -> Integer -> Bool
ambosSon0 x y = x==0 && y==0

ambosSon0PM :: Integer -> Integer -> Bool
ambosSon0PM 0 0 = True
ambosSon0PM _ _ = False


mismoIntervalo :: Float -> Float -> Bool
mismoIntervalo x y | (x<=3 && y<=3)||(x>7 && y>7) = True
                   | (x > 3 && x <= 7) && (y>3 && y <= 7) = True
                   | otherwise = False

sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos a b c | (a /= b) && (b /= c) && (a /= c) = a + b + c
                    | (a == b ) && (a /= c) = c
                    | (a == c ) && (a /= b) = b
                    | (c == b ) && (a /= c) = a
                    | (a == b ) && (a == c) = 0


esMultiploDe :: Int -> Int -> Bool
esMultiploDe n m = mod n m == 0


digitoUnidades :: Int -> Int
digitoUnidades a | absoluto a <= 9 = a
                 | mod a 10 == 0 = 0
                 | otherwise = digitoUnidades (absoluto a - 10)


digitoDecenas :: Int -> Int
digitoDecenas a = div (mod (absoluto a) 100) 10

--
estanRelacionados :: Int -> Int -> Bool
estanRelacionados a b = mod a b == 0

--
prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt t1 t2 = fst t1 * fst t2 + snd t1 * snd t2

prodIntPM :: (Float, Float) -> (Float, Float) -> Float
prodIntPM (x,y) (w,z) = x*w + y*z


todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor t1 t2 = (fst t1 < fst t2 ) && (snd t1 < snd t2)

todoMenorPM :: (Float, Float) -> (Float, Float) -> Bool
todoMenorPM (x,y) (z,w) = x<z && y<w


distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1,x2) (y1,y2) = sqrt((x1 - y1)^2 + (y1 - y2)^2)


sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (a, b, c) = a+b+c


sumarSoloMultiplos :: (Int,Int,Int) -> Int -> Int
sumarSoloMultiplos (a,b,c) n | esMultiploDe a n && esMultiploDe b n && esMultiploDe c n = a + b + c
                             | esMultiploDe a n && esMultiploDe b n && (not (esMultiploDe c n)) = a + b
                             | esMultiploDe a n && (not (esMultiploDe b n )) && esMultiploDe c n = a + c
                             | not (esMultiploDe a n) && esMultiploDe b n && esMultiploDe c n = b + c
                             | esMultiploDe a n = a
                             | esMultiploDe b n = b
                             | esMultiploDe c n = c
                             | otherwise = 0


posPrimerPar ::  (Int,Int,Int) -> Int
posPrimerPar (a,b,c) | mod a 2 == 0 = 1
                     | mod b 2 == 0 = 2
                     | mod c 2 == 0 = 3
                     | otherwise = 4


crearPar :: a -> b -> (a,b)
crearPar x y = (x,y)

invertir :: (a,b) -> (b,a)
invertir (x,y) = (y,x)

--

f :: Int -> Int 
f n | n<=7 = n*n
    | otherwise = 2*n - 1

g :: Int -> Int
g n | mod n 2 == 0 = div n 2
    | otherwise = 3*n - 1


todosMenores :: (Int, Int, Int) -> Bool
todosMenores (a,b,c) = (f(a) > g (a)) && (f(b) > g(b) ) && (f(c) > g(c))

--
{-
bisiesto :: Int -> Bool
bisiesto n = not (mod n 4 /= 0 || (mod n 100 == 0 && mod n 400 /= 0 ))

-}

bisiesto :: Int -> Bool
bisiesto n | mod n 4 /= 0 || (mod n 100 == 0 && mod n 400 /= 0 ) = False
           | otherwise = True

--

distanciaManhattan :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciaManhattan (x1,x2,x3) (y1,y2,y3) = absolutoR(x1-y1) + absolutoR(x2-y2) + absolutoR(x3-y3)

--

sumaUltimosDosDigitos :: Int -> Int
sumaUltimosDosDigitos x = mod (absoluto x) 10 + mod (div (absoluto x )10) 10 

comparar :: Int -> Int -> Int
comparar a b | sumaUltimosDosDigitos a < sumaUltimosDosDigitos b = 1
             | sumaUltimosDosDigitos a > sumaUltimosDosDigitos b = -1
             | sumaUltimosDosDigitos a == sumaUltimosDosDigitos b = 0

