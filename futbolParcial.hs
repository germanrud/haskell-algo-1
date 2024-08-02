sumatoria :: (Num t) => [t] -> t
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

golesDeNoGoleadores :: [(String,String)] -> [Int] -> Int -> Int
golesDeNoGoleadores lista goles total = total - sumatoria goles

--
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece x [] = False
pertenece x l = x == head l || pertenece x (tail l)

perteneceATupla :: (Eq t) => t -> [(t,t)] ->Bool
perteneceATupla x [] = False
perteneceATupla x ((a,b):xs) = x == a || x == b || perteneceATupla x xs 

equiposValidos :: [(String,String)] -> Bool
equiposValidos [] = True
equiposValidos ((a,b):xs) = a/=b && not (perteneceATupla a xs) && not (perteneceATupla b xs) && equiposValidos xs

--
cantGolesGoleador :: String -> [(String,String)] -> [Int] -> Int
cantGolesGoleador x ((a,b):xs) goles | x == b = head goles
                                     | otherwise = cantGolesGoleador x xs (tail goles) 

division :: Int -> Int -> Float
division a b = (fromIntegral a) / (fromIntegral b)

porcentajeDeGoles :: String -> [(String,String)] -> [Int] -> Float
porcentajeDeGoles x lista goles = division (cantGolesGoleador x lista goles) (sumatoria goles)

--

goleadores :: [(String,String)] -> [String]
goleadores [] = []
goleadores ((a,b):xs) = b : goleadores xs

botinDeOroAux :: [(String,String)] -> [Int] -> [String] -> String
botinDeOroAux lista goles [x] = x
botinDeOroAux lista goles (a:b:xs) | cantGolesGoleador a lista goles > cantGolesGoleador b lista goles = botinDeOroAux lista goles (a:xs)
                                   | otherwise = botinDeOroAux lista goles (b:xs) 


botinDeOro :: [(String,String)] -> [Int] -> String
botinDeOro lista goles = botinDeOroAux lista goles (goleadores lista)
