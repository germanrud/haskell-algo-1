import Data.Char

esMinuscula :: Char -> Bool
esMinuscula caracter = ord caracter >= ord 'a' && ord caracter <= ord 'z'

--

letraANatural :: Char -> Int
letraANatural c = ord c - ord 'a'

--
{-
desplazar :: Char -> Int -> Char
desplazar c _ | not (esMinuscula c) = c
desplazar c n | ord c + n >= ord 'a' && ord c + n <= ord 'z' = chr (ord c + n)
              | ord c + n > ord 'z' = desplazar c (n-26)
              | ord c + n < ord 'a' = desplazar c (n+26)
-}
desplazar :: Char -> Int -> Char
desplazar c _ | not (esMinuscula c) = c
desplazar c n | esMinuscula (chr(ord c + n)) = chr (ord c + n)
              | ord c + n > ord 'z' = desplazar c (n-26)
              | ord c + n < ord 'a' = desplazar c (n+26)

--

cifrar :: String -> Int -> String
cifrar [] _     = []
cifrar (x:xs) n = desplazar x n: cifrar xs n

--

descifrar :: String -> Int -> String
descifrar frase n = cifrar frase (-n)

--

cifrarListaDesdePos :: [String]  -> Int -> [String]
cifrarListaDesdePos [] _     = []
cifrarListaDesdePos (x:xs) i = cifrar x i : cifrarListaDesdePos xs (i+1)

cifrarLista :: [String] -> [String]
cifrarLista ls = cifrarListaDesdePos ls 0

--

cantidadMinusculas :: String -> Int
cantidadMinusculas [] = 0
cantidadMinusculas (x:xs) | esMinuscula x = 1 + cantidadMinusculas xs
                          | otherwise     = cantidadMinusculas xs


cuantasVecesAparece :: Char -> String -> Int
cuantasVecesAparece _ [] = 0
cuantasVecesAparece c (x:xs) | c == x    = 1 + cuantasVecesAparece c xs
                             | otherwise = cuantasVecesAparece c xs


frecuenciaRecorriendoABC :: String -> Int -> [Float]
frecuenciaRecorriendoABC ls i | i == 26 = []
                              | cantidadMinusculas ls > 0 = (fromIntegral (cuantasVecesAparece (chr (ord 'a' + i)) ls ) / fromIntegral (cantidadMinusculas ls)) * 100 : frecuenciaRecorriendoABC ls (i+1)
                              | otherwise = 0 : frecuenciaRecorriendoABC ls (i+1)


frecuencia :: String -> [Float]
frecuencia ls = frecuenciaRecorriendoABC ls 0

--

maximo :: (Ord t) => [t] -> t
maximo [x] = x
maximo (x:y:xs) | x >= y = maximo (x:xs)
                | otherwise = maximo (y:xs)

enQuePosicionEsta :: (Eq t) => t -> [t] -> Int    --si esta mas de una vez tira la posicion mas cerca de 0
enQuePosicionEsta x (y:ys) | x == y = 0
                           | otherwise = 1 + enQuePosicionEsta x ys


cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente frase n  = (chr ((enQuePosicionEsta (maximo lista) lista)  + ord 'a') , maximo lista)
                              where lista = frecuencia (cifrar frase n)

--

esDescifradoRecorriendoI :: String -> String -> Int -> Bool
esDescifradoRecorriendoI s1 s2 i | i == 26 = False
                                 | cifrar s1 i == s2 = True
                                 | otherwise = esDescifradoRecorriendoI s1 s2 (i+1)


esDescifrado :: String -> String -> Bool
esDescifrado s1 s2 = esDescifradoRecorriendoI s1 s2 0 

--

descifradosDeAUno :: [String] -> [(String, String)]
descifradosDeAUno [x] = []
descifradosDeAUno (x:y:xs) | esDescifrado x y = (x,y) : (y,x) : descifradosDeAUno (x:xs)
                           | otherwise = descifradosDeAUno (x:xs)


todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados [] = []
todosLosDescifrados (x:xs) = descifradosDeAUno (x:xs) ++ todosLosDescifrados xs

--

elementoIesimoContador :: (Eq t) => Int -> Int -> [t] -> t
elementoIesimoContador i k (x:xs) | i == k = x
                                  | otherwise = elementoIesimoContador i (k+1) xs


elementoIesimo :: (Eq t) => Int -> [t] -> t
elementoIesimo i lista = elementoIesimoContador i 0 lista


expandirClaveContador :: String -> Int -> Int -> String
expandirClaveContador clave n i | n == i = []
                                | otherwise = elementoIesimo (i `mod` (length clave)) clave : expandirClaveContador clave n (i+1)


expandirClave :: String -> Int -> String
expandirClave clave n = expandirClaveContador clave n 0

--

cifrarVigenereClaveSinExpandir :: String -> String -> String
cifrarVigenereClaveSinExpandir [] _ = []
cifrarVigenereClaveSinExpandir (x:xs) (y:ys) = desplazar x (letraANatural y) : cifrarVigenereClaveSinExpandir xs ys


cifrarVigenere :: String -> String -> String
cifrarVigenere s clave = cifrarVigenereClaveSinExpandir s (expandirClave clave (length s))

--

desCifrarVigenereClaveSinExpandir :: String -> String -> String
desCifrarVigenereClaveSinExpandir [] _ = []
desCifrarVigenereClaveSinExpandir (x:xs) (y:ys) = desplazar x (-(letraANatural y)) : desCifrarVigenereClaveSinExpandir xs ys


descifrarVigenere :: String -> String -> String
descifrarVigenere s clave = desCifrarVigenereClaveSinExpandir s (expandirClave clave (length s))

--

valorAbsoluto :: Int -> Int
valorAbsoluto n | n >= 0 = n
                | otherwise = - n

distanciaVigenere :: String -> String -> Int
distanciaVigenere [] [] = 0
distanciaVigenere (x:xs) (y:ys) = valorAbsoluto ((letraANatural x) - (letraANatural y)) + distanciaVigenere xs ys


peorCifrado :: String -> [String] -> String
peorCifrado _ [x] = x
peorCifrado s (x:y:xs) | distanciaVigenere s (cifrarVigenere s x) <= distanciaVigenere s (cifrarVigenere s y)  = peorCifrado s (x:xs)
                       | otherwise = peorCifrado s (y:xs)

--

chequearPalabraConDistintasClaves :: String -> [String] -> String -> [(String, String)]
chequearPalabraConDistintasClaves s [] cifrado = []
chequearPalabraConDistintasClaves s (x:xs) cifrado | (cifrarVigenere s x) == cifrado = (s, x) : chequearPalabraConDistintasClaves s xs cifrado
                                                   | otherwise = chequearPalabraConDistintasClaves s xs cifrado

combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]
combinacionesVigenere [] claves cifrado = [] 
combinacionesVigenere (x:xs) claves cifrado = (chequearPalabraConDistintasClaves x claves cifrado) ++ combinacionesVigenere xs claves cifrado
