zipi :: [a] -> [b] -> [(a,b)]
zipi [] _ = []
zipi _ [] = []
zipi l1 l2 = (head l1 , head l2) : zipi (tail l1) (tail l2)


concatenar :: [Int] -> [Int] -> [Int]
concatenar [] l2 = l2
concatenar l1 l2 = head l1 : concatenar (tail l1) l2

--dada lista de enteros, devuelve lista de los suc

mapSucesor :: [Int] -> [Int]
mapSucesor [] = []
mapSucesor (x:xs) = x+1 : mapSucesor xs

-- dada dos listas de enteros devuelve lista donde el elem n es el max entre el elem n de la l1 y l2

zipiMaximos :: [Int] -> [Int] -> [Int]
zipiMaximos [] _ = []
zipiMaximos _ [] = []
zipiMaximos l1 l2 | head l1 >= head l2 = head l1 : zipiMaximos (tail l1) (tail l2)
                  | otherwise = head l2 : zipiMaximos (tail l1) (tail l2)

-- 