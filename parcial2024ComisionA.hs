hayQueCodificar :: Char -> [(Char,Char)] -> Bool
hayQueCodificar c [] = False
hayQueCodificar c ((a,b):xs) = c == a || hayQueCodificar c xs

--

cuantasVecesAparece :: Char -> [Char] -> Int
cuantasVecesAparece c [] = 0
cuantasVecesAparece c (x:xs) | c == x = 1 + cuantasVecesAparece c xs
                             | otherwise = cuantasVecesAparece c xs

cuantasVecesHayQueCodificar :: Char -> [Char] -> [(Char,Char)] -> Int
cuantasVecesHayQueCodificar c frase mapeo | not (hayQueCodificar c mapeo) = 0
                                          | otherwise = cuantasVecesAparece c frase

--

laQueMasHayQueCodificar :: [Char] -> [(Char,Char)] -> Char
laQueMasHayQueCodificar [x] mapeo = x
laQueMasHayQueCodificar (x:y:xs) mapeo | cuantasVecesHayQueCodificar x (x:y:xs) mapeo >= cuantasVecesHayQueCodificar y (x:y:xs) mapeo
                                           = laQueMasHayQueCodificar (x:xs) mapeo
                                       | otherwise = laQueMasHayQueCodificar (y:xs) mapeo

--

laQueAcompania :: Char -> [(Char,Char)] -> Char
laQueAcompania c ((a,b):xs) | c == a = b 
                            | otherwise = laQueAcompania c xs


codificarFrase :: [Char] -> [(Char,Char)] -> [Char]
codificarFrase [] mapeo = []
codificarFrase (x:xs) mapeo | hayQueCodificar x mapeo = (laQueAcompania x mapeo) : codificarFrase xs mapeo
                            | otherwise = x : codificarFrase xs mapeo