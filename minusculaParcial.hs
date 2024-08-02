import Data.Char

esMinuscula :: Char -> Bool
esMinuscula c = ord c >= 97 && ord c <= 122

cantMinuscula :: [Char] -> Int
cantMinuscula [] = 0
cantMinuscula (x:xs) | esMinuscula x = 1 + cantMinuscula xs
                     | 1==1 = cantMinuscula xs

--

maximoCambios :: [String] -> String
maximoCambios [x] = x
maximoCambios (x:y:xs) | cantMinuscula x >= cantMinuscula y = maximoCambios (x:xs)
                       | otherwise = maximoCambios (y:xs)

maximoCambios2 :: [String] -> String
maximoCambios2 [x] = x
maximoCambios2 (x:xs) | cantMinuscula x >= cantMinuscula (maximoCambios2 xs) = x
                      | otherwise = maximoCambios2 xs

--

desplazar :: Char -> Int -> Char
desplazar c n | ord c < ord 'a' || ord c > ord 'z' = c
              | n >= 0 && (ord c) + n > ord 'z' = chr (ord 'a' - 1  + n - (ord 'z' - ord c))
              | n >= 0 = chr (ord c + n)
              | n < 0 && (ord c) + n < ord 'a' = chr (ord 'z' +1 - ((-n) - (ord c - ord 'a')))
              | otherwise = chr (ord c + n)

--

codificar :: String -> Int -> String
codificar [] _     = [] 
codificar (x:xs) n | esMinuscula x = (desplazar x n) : codificar xs n
                   | otherwise = x : codificar xs n

--

decodificar :: String -> Int -> String
decodificar [] _    = []
decodificar (x:xs) n | esMinuscula x = (desplazar x (-n)) : decodificar xs n
                     | otherwise = x : decodificar xs n

--
