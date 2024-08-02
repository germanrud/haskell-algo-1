{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece x [] = False
pertenece x l = x == head l || pertenece x (tail l)



relacionesValidas :: [(String,String)] -> Bool
relacionesValidas [] = True
relacionesValidas ((a,b):xs) = (a /= b) && ((not (pertenece (a,b) xs)) && (not (pertenece (b,a) xs))) && relacionesValidas xs


relacionesValidas' :: [(String,String)] -> Bool
relacionesValidas' [] = True
relacionesValidas' ((a,b):xs) | (a==b) || ((pertenece (a,b) xs) || ((pertenece (b,a) xs))) = False
                              | otherwise = relacionesValidas' xs
--

quitar :: (Eq t) => t -> [t] -> [t]
quitar x [] = []
quitar x l | x == head l  = tail l
           | otherwise = head l : quitar x (tail l)


quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos x [] = []
quitarTodos x l | pertenece x l = quitarTodos x (quitar x l)
                | otherwise = l


eliminarRepetidos ::(Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | pertenece x xs = x: eliminarRepetidos (quitarTodos x xs)
                         | otherwise = x: eliminarRepetidos xs

--mas facil
sacarRepetidos :: (Eq t) => [t] -> [t]
sacarRepetidos [] = []
sacarRepetidos (x:xs) | pertenece x xs = sacarRepetidos xs
                      | otherwise = x : sacarRepetidos xs


personasAux:: [(String,String)] -> [String]
personasAux [] = []
personasAux ((a,b):xs) = a:b:personasAux xs

personas :: [(String,String)] -> [String]
personas l = eliminarRepetidos (personasAux l)

--

amigosDe :: String -> [(String,String)] -> [String]
amigosDe nombre [] = []
amigosDe nombre ((a,b):xs) | nombre == a = b: amigosDe nombre xs
                           | nombre == b = a: amigosDe nombre xs
                           | otherwise = amigosDe nombre xs

--

longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

cantAmigos :: String -> [(String,String)] -> Int
cantAmigos nombre l = longitud (amigosDe nombre l)


maximo :: [Int] -> Int
maximo l | longitud l == 1 = head l
         | head l >= maximo (tail l) = head l
         | otherwise = maximo (tail l)



maximoElim :: (Ord t) => [t] -> t
maximoElim [x] = x
maximoElim (x:xs) | x >= (head xs) = maximoElim (x: (tail xs))
                  | otherwise = maximoElim xs



personasConMasAmigosAux :: [(String,String)] -> [String] ->  String
personasConMasAmigosAux _ [x] = x
personasConMasAmigosAux relation (y:ys) | cantAmigos y relation >= cantAmigos (head ys) relation = personasConMasAmigosAux relation (y: (tail ys))
                                        | otherwise = personasConMasAmigosAux relation ys

personasConMasAmigos :: [(String,String)] -> String
personasConMasAmigos relation = personasConMasAmigosAux relation (personas relation)
{-
personasANumerosAux :: [(String,String)] -> [String] -> [Int]
personasANumerosAux _ [] = []
personasANumerosAux l people = cantAmigos (head people) l : personasANumerosAux l (tail people)

personasANumeros :: [(String,String)] -> [Int]
personasANumeros l = personasANumerosAux l (personas l)


enQuePosAparecePorPrimeraVezNAux ::(Eq t) => t -> Int -> [t] -> Int
enQuePosAparecePorPrimeraVezNAux x i l | (head l) == x = i
                                       | otherwise = enQuePosAparecePorPrimeraVezNAux x (i+1) (tail l) 

enQuePosAparecePorPrimeraVez :: (Eq t) => t -> [t] -> Int
enQuePosAparecePorPrimeraVez x l = enQuePosAparecePorPrimeraVezNAux x 1 l


dameElElemIAux :: Int -> Int -> [t] -> t
dameElElemIAux i k l | i == k = (head l)
                     | otherwise = dameElElemIAux i (k+1) (tail l) 

dameElElemI :: Int -> [t] -> t
dameElElemI i l = dameElElemIAux i 1 l


personasConMasAmigos :: [(String,String)] -> String
personasConMasAmigos l = dameElElemI (enQuePosAparecePorPrimeraVez (maximo (personasANumeros l)) (personasANumeros l)) (personas l)

-}
