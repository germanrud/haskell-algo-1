divisoresPropiosDesde :: Int -> Int -> [Int]
divisoresPropiosDesde n i | i == n = []
                          | n `mod` i == 0 = i : divisoresPropiosDesde n (i+1)
                          | otherwise = divisoresPropiosDesde n (i+1)

divisoresPropios :: Int -> [Int]
divisoresPropios n = divisoresPropiosDesde n 1 

--

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

sonAmigos :: Int -> Int -> Bool
sonAmigos n m = sumatoria (divisoresPropios n) == m && sumatoria (divisoresPropios m) == n && n /= m

--

esPerfecto :: Int -> Bool
esPerfecto n = sumatoria (divisoresPropios n) == n


losPrimerosNPerfectosDesdeAux :: Int -> Int -> Int -> [Int]
losPrimerosNPerfectosDesdeAux n i k | i == n = []
                                    | esPerfecto k = k : losPrimerosNPerfectosDesdeAux n (i+1) (k+1)
                                    | otherwise = losPrimerosNPerfectosDesdeAux n i (k+1)

losPrimerosNPerfectos :: Int -> [Int]
losPrimerosNPerfectos n = losPrimerosNPerfectosDesdeAux n 0 1

--
listaAmigosAux :: Int -> [Int] -> [(Int,Int)]
listaAmigosAux n [] = []
listaAmigosAux n (x:xs) | sonAmigos n x = (n,x) : listaAmigosAux n xs
                        | otherwise = listaAmigosAux n xs


listaDeAmigos :: [Int] -> [(Int,Int)]
listaDeAmigos [] = []
listaDeAmigos (x:xs) = listaAmigosAux x (xs) ++ listaDeAmigos xs

