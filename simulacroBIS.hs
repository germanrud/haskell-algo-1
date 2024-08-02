

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs 



relacionesValidas :: [(String,String)] -> Bool
relacionesValidas [] = True
relacionesValidas ((a,b):xs) = a/=b && not (pertenece (a,b) xs) && not (pertenece (b,a) xs) && relacionesValidas xs

relacionesValidas' :: [(String,String)] -> Bool
relacionesValidas' [] = True
relacionesValidas' ((a,b):xs) | a==b || pertenece (a,b) xs || pertenece (b,a) xs = False
                              | otherwise = relacionesValidas' xs 
--

quitarRepetidos :: (Eq t) => [t] -> [t]
quitarRepetidos [] = []
quitarRepetidos (x:xs) | pertenece x xs = quitarRepetidos xs
                       | otherwise = x : quitarRepetidos xs

personasConRepe :: [(String,String)] -> [String]
personasConRepe [] = []
personasConRepe ((a,b):xs) = a:b: (personasConRepe xs)

personas :: [(String,String)] -> [String]
personas l = quitarRepetidos (personasConRepe l)

--challenge hacerla sin quitarRepe

personasAux :: [(String,String)] -> [String] -> [String]
personasAux [] _ = []
personasAux ((a,b):xs) lista | (not (pertenece a lista)) && not ((pertenece b lista)) = a:b: (personasAux xs (a:b:lista))
                             | (not (pertenece a lista)) && (pertenece b lista) = a: personasAux xs (a:lista)
                             | (pertenece a lista) && (not (pertenece b lista)) = b: personasAux xs (b:lista)
                             | (pertenece a lista) && (pertenece b lista) = personasAux xs lista

personasAux' :: [(String,String)] -> [String] -> [String]
personasAux' [] l = l
personasAux' ((a,b):xs) lista | (not (pertenece a lista)) && not ((pertenece b lista)) = (personasAux' xs (a:b:lista))
                             | (not (pertenece a lista)) && (pertenece b lista) = personasAux' xs (a:lista)
                             | (pertenece a lista) && (not (pertenece b lista)) = personasAux' xs (b:lista)
                             | (pertenece a lista) && (pertenece b lista) = personasAux' xs lista




--

amigosDe :: String -> [(String,String)] -> [String]
amigosDe x [] = []
amigosDe x ((a,b):xs) | x == a = b: amigosDe x xs
                      | x == b = a: amigosDe x xs
                      | otherwise = amigosDe x xs

-- 
longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


cantidadAmigos :: String -> [(String,String)] -> Int
cantidadAmigos name l = longitud (amigosDe name l)

personasConMasAmisAux :: [(String,String)] -> [String] -> String
personasConMasAmisAux relaciones [y] = y
personasConMasAmisAux relaciones (y:y2:ys) | (cantidadAmigos y relaciones) > (cantidadAmigos y2 relaciones) = personasConMasAmisAux relaciones (y:ys)
                                           | otherwise = personasConMasAmisAux relaciones (y2:ys)

personasConMasAmis :: [(String,String)] -> String
personasConMasAmis relaciones = personasConMasAmisAux relaciones (personas relaciones)

--ej para practicar:
-- cantidadApariciones :: Int -> [Int] -> Int cantidad que aparece un entero n en una lista de numeros

cantidadApariciones :: Int -> [Int] -> Int 
cantidadApariciones n [] = 0
cantidadApariciones n (x:xs) | n == x = 1 + cantidadApariciones n xs
                             | otherwise = cantidadApariciones n xs


cantidadAparicionesTupla :: Int -> [(Int,Int)] -> Int
cantidadAparicionesTupla x [] = 0
cantidadAparicionesTupla x ((a,b):xs) | x==a && x==b = 2 + cantidadAparicionesTupla x xs
                                      | x==a && x/=b = 1 + cantidadAparicionesTupla x xs
                                      | x==a && x/=b = 1 + cantidadAparicionesTupla x xs
                                      | otherwise = cantidadAparicionesTupla x xs