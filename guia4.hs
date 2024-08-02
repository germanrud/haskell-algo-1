

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1)


fibonacci :: Int -> Int 
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)


sumaPrimerosN :: Int -> Int
sumaPrimerosN 1 = 1
sumaPrimerosN n = n + sumaPrimerosN(n-1)


parteEntera :: Float -> Int
parteEntera x | x<1 && x>=0 = 0
              | x>= 1 = parteEntera (x-1) + 1
              | x < 0 =  - parteEntera (-x + 1)


esDivisible :: Int -> Int -> Bool
esDivisible 0 k = True
esDivisible n k | n < k = False
esDivisible n k = esDivisible (n-k) k


nEsimoImpar :: Int -> Int
nEsimoImpar n = 2*n - 1

sumaImpares :: Int -> Int 
sumaImpares 1 = 1
sumaImpares n = nEsimoImpar n + sumaImpares(n-1)


medioFact :: Int -> Int
medioFact 0 = 1
medioFact 1 = 1
medioFact n = n * medioFact (n-2)


sumaDigitos :: Int -> Int
sumaDigitos n | n <= 9 = n
              | otherwise = mod n 10 + sumaDigitos (div n 10)



todosDigitosIguales :: Int -> Bool
todosDigitosIguales n | n<= 9 = n == mod n 10
                      | otherwise = (mod n 10 == mod (div n 10 ) 10) && todosDigitosIguales (div n 10)


-- est no se pq no funciona...
--iesimoDigito :: Int -> Int -> Int
--iesimoDigito n i = mod (div n 10^(cantDigitos (n) - i)) 10


iesimoDigito :: Int -> Int -> Int
iesimoDigito n i | i == cantDigitos n = mod n 10
                 | otherwise = iesimoDigito (div n 10) i


cantDigitos :: Int -> Int
cantDigitos n | n<= 9 = 1
              | otherwise = 1 + cantDigitos (div n 10)



sacarPrimeroYUltimo :: Int -> Int
sacarPrimeroYUltimo n = (n `mod` (10^(cantDigitos n - 1))) `div` 10



esCapicua :: Int -> Bool
esCapicua n | cantDigitos n <= 1 = True
            | cantDigitos n == 2 = todosDigitosIguales n
            | otherwise = iesimoDigito n 1 == iesimoDigito n (cantDigitos n) && esCapicua (sacarPrimeroYUltimo n)

---

f1 :: Int -> Int 
f1 0 = 1
f1 n = 2^n + f1 (n-1)

f2 :: Int -> Float -> Float
f2 1 q = q
f2 n q = q^n + f2 (n-1) q

f2' :: Int -> Int -> Int
f2' 1 q = q
f2' n q = q^n + f2' (n-1) q

f3 :: Int -> Float -> Float
f3 1 q = q
f3 n q = q^(2*n - 1) + q^(2*n) + f3 (n-1) q

f4 :: Int -> Float -> Float
f4 1 q = q
f4 n q = q^(2*n - 1) + q^(2*n) - q^(n-1)+ f4 (n-1) q 

--

eAprox :: Int -> Float
eAprox 0  = 1
eAprox n = (1 / fromIntegral (factorial n) ) + eAprox (n-1)

e :: Float
e = eAprox 10
--

an :: Int -> Float
an 1 = 2
an n = 2 + 1/ an (n-1)

raizDe2Aprox :: Int -> Float
raizDe2Aprox n = an n - 1
 --

f5 :: Int -> Int -> Int
f5 1 m = f2' m 1
f5 n m = f2' m n + f5 (n-1) m
------------------------------------
-- otra forma, en clase moviendo dos parametros onda pitagoras
-- i fijo
sumaPotClase :: Int -> Int -> Int -> Int -> Int
sumaPotClase n m i j | j > m = 0
                     | otherwise = i^j + sumaPotClase n m i (j+1)

--hUnit 
-- j fijo
sumaJ :: Int -> Int -> Int -> Int -> Int
sumaJ  n m i j | i > n = 0
               | otherwise = sumaPotClase n m i j + sumaJ n m (i+1) j

sumaPOTCLASE n m = sumaJ n m 1 1


----

sumaPotencias' :: Int -> Int -> Int -> Int
sumaPotencias' q n m = f2' n q * f2' m q 

sumaPotencias :: Float -> Int -> Int -> Float
sumaPotencias q 1 m =  q * f2 m q
sumaPotencias q n m = (q^n) * f2 m q + sumaPotencias q (n-1) m

--
sumaInversos :: Int -> Float
sumaInversos 1 = 1
sumaInversos m = 1/(fromIntegral m) + sumaInversos (m-1)

sumaRacionales :: Int -> Int -> Float
sumaRacionales 1 m = sumaInversos m 
sumaRacionales n m = fromIntegral n * sumaInversos m + sumaRacionales (n-1) m 

sumaRacionales' :: Int -> Int -> Float
sumaRacionales' n m = fromIntegral (sumaPrimerosN n) * sumaInversos m
--

minimoDivisorDesde :: Int -> Int -> Int
minimoDivisorDesde n m   | n == m = m
                         | n ` mod` m == 0 && m /= 1 = m 
                         | otherwise = minimoDivisorDesde n (m+1)



menorDivisor :: Int -> Int
menorDivisor n = minimoDivisorDesde n 1


esPrimo :: Int -> Bool 
esPrimo n = n == menorDivisor n

esMultiploDe :: Int -> Int -> Bool
esMultiploDe n m = mod n m == 0

minimoDivisorEnComunDesde :: Int -> Int -> Int -> Int
minimoDivisorEnComunDesde n m k | k == min n m && esMultiploDe (max n m) (min n m) = min n m
                                | k == min n m && not (esMultiploDe (max n m) (min n m)) = 1
                                | (n `mod` k == 0) && (m `mod` k == 0) = k
                                | otherwise = minimoDivisorEnComunDesde n m (k+1)

sonCoprimos :: Int -> Int -> Bool 
sonCoprimos n m = minimoDivisorEnComunDesde n m 2 == 1 




minimoPrimoDesde :: Int -> Int
minimoPrimoDesde n | esPrimo n = n
                   | otherwise = minimoPrimoDesde (n+1)



nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (nEsimoPrimo (n-1) + 1)

--

esFiboDesde :: Int -> Int -> Bool
esFiboDesde n k | fibonacci k == n = True
                | fibonacci k > n = False
                | fibonacci k < n = esFiboDesde n (k+1) 

esFibonacci :: Int -> Bool
esFibonacci n = esFiboDesde n 1

--

todosImpares :: Int -> Bool
todosImpares n | cantDigitos n == 1 = n `mod` 2 == 1
               | otherwise = mod (mod n 10) 2 == 1 && todosImpares (div n 10)


{-
proximoParDesde :: Int -> Int -> Int
proximoParDesde n i | todosImpares n = -1
                    | (iesimoDigito n i) `mod` 2 == 0 = iesimoDigito n i
                    | otherwise = proximoParDesde n (i+1)


proximoPar :: Int -> Int
proximoPar n = proximoParDesde n 1
 -}


-- si desde aca en adelante hay un par mayor o igual, dame true, sino false
proximoParMayorDesde :: Int -> Int -> Bool
proximoParMayorDesde n i  | cantDigitos n < i = False
                          | (iesimoDigito n i) `mod` 2 == 0  && (iesimoDigito n i) > iesimoDigito n 1  = True
                          | otherwise = proximoParMayorDesde n (i+1)


mayorDigitoParDesde :: Int -> Int -> Int
mayorDigitoParDesde n i | todosImpares n = -1
                        | (iesimoDigito n i) `mod` 2 == 0 && not (proximoParMayorDesde n (i+1)) = iesimoDigito n i
                        | otherwise = mayorDigitoParDesde n (i+1)


mayorDigitoPar :: Int -> Int
mayorDigitoPar n = mayorDigitoParDesde n 1

-- otra opc mas facil

estaJenN :: Int -> Int -> Int -> Bool
estaJenN n i j | cantDigitos n < i = False
               | iesimoDigito n i == j = True
               | otherwise = estaJenN n (i+1) j


mayorParDesdeJ :: Int -> Int -> Int
mayorParDesdeJ n j| j < 0 = -1
                  | estaJenN n 1 j = j
                  | otherwise = mayorParDesdeJ n (j-2)


mayorDigitoPar' :: Int -> Int
mayorDigitoPar' n = mayorParDesdeJ n 8

--

sumaPrimerosNPrimos :: Int -> Int
sumaPrimerosNPrimos 1 = 2
sumaPrimerosNPrimos n = sumaPrimerosNPrimos (n-1) + nEsimoPrimo n

esSumaInicialDePrimosDesde :: Int -> Int -> Bool
esSumaInicialDePrimosDesde n k | sumaPrimerosNPrimos k > n = False
                               | sumaPrimerosNPrimos k == n = True
                               | sumaPrimerosNPrimos k < n = esSumaInicialDePrimosDesde n (k+1)

--

sumaDivisoresDesde :: Int -> Int -> Int
sumaDivisoresDesde n i | n == i = n 
                       | esDivisible n i = i + sumaDivisoresDesde n (i + 1)
                       | otherwise = sumaDivisoresDesde n (1 + i)



sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n i | i == 1 = 1
                       | esDivisible n i = i + sumaDivisoresHasta n (i - 1)
                       | otherwise = sumaDivisoresHasta n (i - 1)



sumaDivisores' :: Int -> Int
sumaDivisores' n = sumaDivisoresHasta n n


sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresDesde n 1


{-
hayMasDivDesde :: Int -> Int -> Int -> Bool
hayMasDivDesde n1 n2 i | n1 + i > n2 = False
                       | n1 + 1 == n2 = sumaDivisores(n2) > sumaDivisores(n1) 
                       | sumaDivisores (n1 + i) > sumaDivisores n1 = True
                       | otherwise = hayMasDivDesde n1 n2 (i+1)

en esta forma agregue una i innecesaria, que siemore la hacia valer cero dsps y me complico un poco lo que seguia y 
la cambie por la opcB que es mas simple
-}

hayMasDivOPCIONB :: Int -> Int  -> Bool
hayMasDivOPCIONB n1 n2 | n1 > n2 = False
                       | sumaDivisores n2 > sumaDivisores n1 = True
                       | otherwise = hayMasDivOPCIONB n1 (n2 - 1) 

{-
mayorValorOPCIONB :: Int -> Int -> Int
mayorValorOPCIONB n1 n2 | n1 == n2 = n1
                        | (sumaDivisores n2 > sumaDivisores n1 ) && not (hayMasDivOPCIONB n1 n2) = n2
                        | otherwise = mayorValorOPCIONB n1 (n2-1)
-}

mayorValorDesde :: Int -> Int -> Int -> Int
mayorValorDesde n1 n2 i | n1 + i == n2 && sumaDivisores(n1) <= sumaDivisores(n2) = n2
                        | n1 + i == n2 && sumaDivisores(n1) > sumaDivisores(n2) = n1
                        | sumaDivisores(n1 + i) > sumaDivisores(n1) && not (hayMasDivOPCIONB (n1+i) n2)
                            = n1 + i
                        | otherwise = mayorValorDesde n1 n2 (i + 1)

--esto hace: anda comparando los sumaDiv de los numeros entre n1 y n2, cuando encuentres el primero
--que cumpla que es mayor al sumDiv n1  y que a no trnga nadie con una sumDiv mas grande a la derecha
-- ahi tenes al max :)

tomaValorMax :: Int -> Int -> Int
tomaValorMax n1 n2 = mayorValorDesde n1 n2 0

----

--p fijo
pFijoQMueve :: Int -> Int -> Int -> Int -> Int -> Int
pFijoQMueve n m r p q | q>m = 0
                      | (p^2 + q^2 <= r^2 && q <= m) = 1 + pFijoQMueve n m r p (q+1)
                      | otherwise = pFijoQMueve n m r p (q+1)


qFijoPMueve :: Int -> Int -> Int -> Int -> Int -> Int
qFijoPMueve n m r p q | p>n = 0
                      | otherwise = pFijoQMueve n m r p q  + qFijoPMueve n m r (p+1) q

pitagoras :: Int -> Int -> Int -> Int
pitagoras n m r = qFijoPMueve n m r 0 0

--

--menor fact desde, clase de taller

menorFactDesdeDesde :: Int -> Int -> Int
menorFactDesdeDesde m n | factorial n >= m = factorial n 
                        | otherwise = menorFactDesdeDesde m (n+1)


menorFactDesde :: Int -> Int
menorFactDesde m = menorFactDesdeDesde m 1

--
-- ej extra suma de dos primos

--b fijo

sumaSimple :: Int -> Int -> Int -> Bool
sumaSimple n a b | (nEsimoPrimo a)  > n = False
                 | (nEsimoPrimo a) + b == n = True
                 | otherwise = sumaSimple n (a+1)  b


sumaDoble :: Int -> Int -> Int -> Bool
sumaDoble n a b | nEsimoPrimo b > n = False
                | sumaSimple n a (nEsimoPrimo b) = True
                | otherwise = sumaDoble n a (b+1)


esSumaDeDosPrimos :: Int -> Bool
esSumaDeDosPrimos n = sumaDoble n 1 1


goldbach :: Int -> Bool 
goldbach n | n <= 4 = True
           | otherwise = esSumaDeDosPrimos n && goldbach (n - 2)
--

primosGemAux :: Int -> Int -> Int
primosGemAux n i | nEsimoPrimo i + 2 > n = 0
                 | nEsimoPrimo i + 2 == nEsimoPrimo (i+1) = 1 + primosGemAux n (i+1)
                 | otherwise = primosGemAux n (i+1)

primosGem :: Int -> Int
primosGem n = primosGemAux n 1
--

nEsimosGemelosAux :: Int -> Int -> Int -> (Int,Int)
nEsimosGemelosAux num i k | i == num = (nEsimoPrimo (k-1), nEsimoPrimo k)
                          | nEsimoPrimo k + 2 == nEsimoPrimo (k+1) = nEsimosGemelosAux num (i+1) (k+1)
                          | otherwise = nEsimosGemelosAux num i (k+1)

nEsimosGemelos :: Int -> (Int,Int)
nEsimosGemelos n = nEsimosGemelosAux n 0 1


proxPrimosGemAux :: Int -> Int -> (Int,Int)
proxPrimosGemAux n i | fst (nEsimosGemelos i) > n = nEsimosGemelos i
                     | otherwise = proxPrimosGemAux n (i+1)

proxPrimosGem :: Int -> (Int,Int)
proxPrimosGem n = proxPrimosGemAux n 1
--

listaCollatz :: Int -> [Int]
listaCollatz an | an == 1 = [an]
                | an `mod` 2 == 0 = an : listaCollatz (an `div` 2)
                | otherwise = an : listaCollatz (3*an + 1)

longitud' :: [Int] -> Int
longitud' [] = 0
longitud' (x:xs) = 1 + longitud' xs

largoSecuencia :: Int -> Int
largoSecuencia n = longitud' (listaCollatz n) - 1

--

maximoLista :: [Int] -> Int
maximoLista [x] = x 
maximoLista (x:xs) | x >= head xs = maximoLista (x: (tail xs))
                   | otherwise = maximoLista xs



hayAlguienMayorDesdeAux :: Int -> Int -> Bool
hayAlguienMayorDesdeAux n i | i == 10000 = False
                            | otherwise = (largoSecuencia i > largoSecuencia n) || hayAlguienMayorDesdeAux n (i+1)

hayAlguienMayorDesde :: Int -> Bool
hayAlguienMayorDesde n = hayAlguienMayorDesdeAux n n


mayorQueLosAnterioresAux :: Int -> Int -> Bool
mayorQueLosAnterioresAux n i | i == 1 = True
                             | otherwise = (largoSecuencia n >= largoSecuencia i) && mayorQueLosAnterioresAux n (i-1)

mayorQueLosAnteriores n = mayorQueLosAnterioresAux n n

maximoSecuenciaCollatz :: Int -> Int
maximoSecuenciaCollatz n | (mayorQueLosAnteriores n) && not (hayAlguienMayorDesde n) = n
                         | otherwise = maximoSecuenciaCollatz (n+1)