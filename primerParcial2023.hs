division :: Int -> Int -> Float
division a b = (fromIntegral a) / (fromIntegral b)

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

porcentajeDeVotosAfirmativos :: [(String, String)] -> [Int] -> Int  -> Float
porcentajeDeVotosAfirmativos formulas votos z = division (sumatoria votos) z 

--

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece x [] = False
pertenece x l = x == head l || pertenece x (tail l)


noApareceEnAlgunaLista :: String -> [(String, String)] -> Bool
noApareceEnAlgunaLista _ [] = True
noApareceEnAlgunaLista k ((a,b):xs) = k /= a && k /= b && noApareceEnAlgunaLista k xs


formulasValidas :: [(String, String)] -> Bool
formulasValidas [] = True
formulasValidas ((a,b):xs) = (a /= b) && noApareceEnAlgunaLista a xs && noApareceEnAlgunaLista b xs && formulasValidas xs

--

cuantosVotos :: String -> [(String, String)] -> [Int] -> Int
cuantosVotos presi ((a,b):xs) votos | presi == a = (head votos)
                                    | otherwise = cuantosVotos presi xs (tail votos)

porcentajeDeVotos :: String -> [(String, String)] -> [Int] -> Float
porcentajeDeVotos presi l votos = fromIntegral (cuantosVotos presi l votos) / fromIntegral (sumatoria votos)

--
longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

maximo :: [Int] -> Int
maximo l | longitud l == 1 = head l
         | head l >= maximo (tail l) = head l
         | otherwise = maximo (tail l)


maximoRaro :: [Int] -> Int
maximoRaro l | head l >= maximo l = head l


proximoPresidente :: [(String, String)] -> [Int] -> String
proximoPresidente ((a,b):xs) votos | cuantosVotos a ((a,b):xs) votos == maximo votos = a
                                   | otherwise = proximoPresidente xs (tail votos)