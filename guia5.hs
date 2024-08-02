module Guia5 where

sumatoria :: (Num t) => [t] -> t
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

longitud' :: [Int] -> Int
longitud' [] = 0
longitud' (x:xs) = 1 + longitud' xs
{-
pertenece :: Int -> [Int] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs
-}

--
longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs
--

ultimo :: [t] -> t
ultimo (x:xs) | longitud (x:xs) == 1 = x
              | otherwise = ultimo xs
--

principio :: [t] -> [t]
principio [] = []
principio (x:xs) | longitud (x:xs) == 1 = []
                 | otherwise = x : (principio xs)

--
reverso :: [t] -> [t]
reverso [] = []
reverso l  = (ultimo l) : reverso (principio l)

--
pertenece :: (Eq t) => t -> [t] -> Bool 
pertenece _ [] = False
pertenece n (x:xs) = n == x || pertenece n xs

--
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales l | longitud l == 1 = True
todosIguales l | longitud l == 2 = pertenece (head l) (tail l) 
todosIguales (x:xs) = x == head xs && todosIguales xs

--
todosDistintos:: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos l | longitud l == 1 = False
                 | longitud l == 2 = not (todosIguales l)
                 | otherwise = not (pertenece (head l) (tail l)) && todosDistintos(tail l)

--  -A ^ B
--              A = pertenece x a xs       B = xs son todos distintos

hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos l | longitud l == 1 = False
hayRepetidos l = pertenece (head l) (tail l) || hayRepetidos (tail l)
-- A U -B
hayRepetidos' :: (Eq t) => [t] -> Bool
hayRepetidos' l = not (todosDistintos l)
--

quitar :: (Eq t) => t -> [t] -> [t]
quitar n []      = [] 
quitar n (x:xs)  | n == x = xs
                 | otherwise = x : (quitar n xs) 

--
quitarTodos :: (Eq t ) => t -> [t] -> [t]
quitarTodos n l | not (pertenece n l) = l
                | otherwise = quitarTodos n (quitar n l)
--

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitarTodos x xs)
--

mismosElementosAux :: (Eq t) => [t] -> [t] -> Bool
mismosElementosAux [] _ = True
mismosElementosAux l1 l2 = (head l1) `pertenece` l2 && mismosElementosAux (tail l1) l2

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos l1 l2 = mismosElementosAux l1 l2 && mismosElementosAux l2 l1
--

mismaListaSiHayMismaCantDeEltos :: (Eq t) => [t] -> [t] -> Bool
mismaListaSiHayMismaCantDeEltos [] [] = True
mismaListaSiHayMismaCantDeEltos l1 l2 = (head l1 == head l2) && mismaListaSiHayMismaCantDeEltos (tail l1) (tail l2)

capicua :: (Eq t) => [t] -> Bool
capicua l = l `mismaListaSiHayMismaCantDeEltos` (reverso l)

--otra forma:
capicua' :: (Eq t) => [t] -> Bool
capicua' l | longitud l <= 1 = True
            | longitud l == 2 = todosIguales l
            | otherwise = (head l) == ultimo l && capicua' (principio (tail (l)))

--

productoria :: [Int] -> Int
productoria [] = 1
productoria l = (head l) * productoria (tail l)

--

maximo :: [Int] -> Int
maximo l | longitud l == 1 = head l
         | head l >= maximo (tail l) = head l
         | otherwise = maximo (tail l)


minimo :: [Int] -> Int
minimo l | longitud l == 1 = head l
         | head l <= minimo (tail l) = head l
         | otherwise = minimo (tail l)

--

sumarN :: Int -> [Int] -> [Int]
sumarN _ [] = []
sumarN n l = ((head l) + n) : sumarN n (tail l)

--

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero l = sumarN (head l) l

--

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo l = sumarN (ultimo l) l 

--

pares :: [Int] -> [Int]
pares [] = []
pares l | (head l) `mod` 2 == 0 = (head l) : pares (tail l)
        | otherwise = pares (tail l)

--
--no se si considerar el 0 como un multiplo de 3
multiplosDeN :: Int -> [Int] -> [Int]
multiplosDeN _ [] = []
multiplosDeN n l | (head l) `mod` n == 0 = (head l) : multiplosDeN n (tail l) 
                 | otherwise = multiplosDeN n (tail l)
--

quitarMinimo :: [Int] -> [Int]
quitarMinimo l = quitar (minimo l) l

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar l = (minimo l) : ordenar (quitarMinimo l)

--

sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos l | longitud l == 1 = l
sacarBlancosRepetidos l | (head l) == ' ' && head (tail l) == ' ' = sacarBlancosRepetidos (tail l)
                        | otherwise = (head l) : sacarBlancosRepetidos (tail l)

--

noEmpezarNiTerminarConEspacios :: [Char] -> [Char]
noEmpezarNiTerminarConEspacios l | l == [] = l
                                 | (head l) == ' ' && (ultimo l) == ' ' = principio (tail l)
                                 | (head l) == ' ' = tail l
                                 | (ultimo l) == ' ' = principio l
                                 | otherwise = l

escribirBien :: [Char] -> [Char]
escribirBien l = noEmpezarNiTerminarConEspacios (sacarBlancosRepetidos l)



contarPalabrasAux :: [Char] -> Int
contarPalabrasAux l | l == [] = 0
                    | head l == ' ' =  1 + contarPalabrasAux (tail l)
                    | otherwise = contarPalabrasAux (tail l)

contarPalabras :: [Char] -> Int
contarPalabras l = contarPalabrasAux (escribirBien l) + 1



contarPalabrasAux2 :: [Char] -> Int
contarPalabrasAux2 [] = 0
contarPalabrasAux2 [x] = 0
contarPalabrasAux2 l | longitud l == 2 = 0
contarPalabrasAux2 (a:b:c:xs) | a/= ' ' && b == ' ' && c/= ' ' = 1 + contarPalabrasAux2 (b:c:xs)
                              | otherwise = contarPalabrasAux2 (b:c:xs)

contarPalabras2 :: [Char] -> Int
contarPalabras2 l = 1 + contarPalabrasAux2 (escribirBien l)
-------

hacerPalabraHastaEspacio :: [Char] -> [Char]
hacerPalabraHastaEspacio [] = []
hacerPalabraHastaEspacio l | head l == ' ' = []
hacerPalabraHastaEspacio l = (head l) : hacerPalabraHastaEspacio (tail l)

-- n <= long de l

quitarPrimerosNAux :: [Char] -> Int  -> Int -> [Char]
quitarPrimerosNAux l n i | i == n = l
                         | otherwise = quitarPrimerosNAux (tail l) n (i+1)


quitarPrimerosN :: [Char] -> Int -> [Char]
quitarPrimerosN l n = quitarPrimerosNAux l n 0


palabras :: [Char] -> [[Char]]
palabras [] = []
palabras l | head l /= ' ' = (hacerPalabraHastaEspacio l) : palabras (quitarPrimerosN l (longitud (hacerPalabraHastaEspacio l)))
           | otherwise = palabras (tail l)



-------

palabraMasLarga :: [Char] -> [Char]
palabraMasLarga [] = []
palabraMasLarga l | longitud (head (palabras l)) >= longitud (palabraMasLarga (tail l)) = head (palabras l)
                  | otherwise = palabraMasLarga (tail l)

--

palabraMasLargaAux2 :: [[Char]] -> [Char]
palabraMasLargaAux2 [x] = x
palabraMasLargaAux2 (x:y:xs) | longitud x > longitud y = palabraMasLargaAux2 (x:xs)
                             | otherwise = palabraMasLargaAux2 (y:xs)

palabraMasLarga2 :: [Char] -> [Char]
palabraMasLarga2 l =  palabraMasLargaAux2 (palabras l)

--

aplanar :: [[Char]] -> [Char]
aplanar [] = []
aplanar (xs:xss) = xs ++ (aplanar xss)


--

aplanarConBlancos :: [[Char]] -> [Char]
aplanarConBlancos [] = []
aplanarConBlancos (xs:xss) = xs ++ [' '] ++ (aplanarConBlancos xss) 

--
hacerNBlancosAux :: Int -> Int -> [Char]
hacerNBlancosAux n i | n==i = []
                     | otherwise = ' ' : hacerNBlancosAux n (i+1)


sacarBlancosAlFinal :: [Char] -> [Char]
sacarBlancosAlFinal [] = []
sacarBlancosAlFinal l | ultimo l == ' ' = sacarBlancosAlFinal (principio l)
                      | otherwise = l


hacerNBlancos :: Int -> [Char]
hacerNBlancos n = hacerNBlancosAux n 0


aplanarConNBlancosAux :: [[Char]] -> Int -> [Char]
aplanarConNBlancosAux [] n = []
aplanarConNBlancosAux (xs:xss) n = xs ++ hacerNBlancos n ++ aplanarConNBlancosAux xss n

aplanarConNBlancos :: [[Char]] -> Int -> [Char]
aplanarConNBlancos l n = sacarBlancosAlFinal (aplanarConNBlancosAux l n)


--

sumaAcumuladaAux :: (Num t) => [t] -> [t]
sumaAcumuladaAux [] = []
sumaAcumuladaAux l = (sumatoria l) : (sumaAcumuladaAux (principio l))

sumaAcumulada :: (Num t) => [t] -> [t]
sumaAcumulada l = reverso (sumaAcumuladaAux l)

--
minimoDivisorDesde :: Int -> Int -> Int
minimoDivisorDesde n m   | n == m = m
                         | n ` mod` m == 0 && m /= 1 = m 
                         | otherwise = minimoDivisorDesde n (m+1)



menorDivisor :: Int -> Int
menorDivisor n = minimoDivisorDesde n 1


esPrimo :: Int -> Bool 
esPrimo n = n == menorDivisor n

minimoPrimoDesde :: Int -> Int
minimoPrimoDesde n | esPrimo n = n
                   | otherwise = minimoPrimoDesde (n+1)

nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (nEsimoPrimo (n-1) + 1)



descoUnNumDesdePrimo :: Int -> Int -> [Int]
descoUnNumDesdePrimo n i | esPrimo n = [n]
                         | n `mod` (nEsimoPrimo i) == 0 = (nEsimoPrimo i) : descoUnNumDesdePrimo (n `div` nEsimoPrimo i) i
                         | otherwise = descoUnNumDesdePrimo n (i+1)




descomponerUnNumeroEnPrimos :: Int -> [Int]
descomponerUnNumeroEnPrimos n = descoUnNumDesdePrimo n 1




descomponerEnPrimos :: [Int] -> [[Int]] 
descomponerEnPrimos [] = []
descomponerEnPrimos l = descomponerUnNumeroEnPrimos (head l) : descomponerEnPrimos (tail l)


