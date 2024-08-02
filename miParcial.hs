

cuantasVecesAparece :: (Eq t) => t -> [t] -> Int
cuantasVecesAparece x [] = 0
cuantasVecesAparece x (y:ys) | x == y = 1 + cuantasVecesAparece x ys
                             | otherwise = cuantasVecesAparece x ys


quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos x [] = []
quitarTodos x (y:ys) | x == y = quitarTodos x ys
                     | otherwise = y : quitarTodos x ys


generarStock :: [[Char]] -> [([Char],Int)]
generarStock [] = []
generarStock (x:xs) = (x , cuantasVecesAparece x (x:xs)) : generarStock (quitarTodos x xs)

--

perteneceATupla :: [Char] -> [([Char],Int)] -> Bool
perteneceATupla x [] = False
perteneceATupla x ((a,b):xs) = x == a || perteneceATupla x xs

stockDeProducto :: [([Char],Int)] -> [Char] -> Int
stockDeProducto ((cosa,cant):xs) producto | not (perteneceATupla producto ((cosa,cant):xs)) = 0
                                          | cosa == producto = cant
                                          | otherwise = stockDeProducto xs producto

--mas facil era esta
stockDeProducto2 :: [([Char],Int)] -> [Char] -> Int
stockDeProducto2 [] producto = 0
stockDeProducto2 ((cosa,cant):xs) producto | cosa == producto = cant
                                           | otherwise = stockDeProducto2 xs producto

--

precioDeProd :: [Char] -> [([Char],Float)] -> Float
precioDeProd producto ((cosa,precio):xs) | producto == cosa = precio
                                         | otherwise = precioDeProd producto xs


dineroEnStock :: [([Char],Int)] -> [([Char],Float)] -> Float
dineroEnStock [] _ = 0
dineroEnStock ((produc,cant):xs) precios = (precioDeProd produc precios) * fromIntegral cant + dineroEnStock xs precios

--

aplicarOferta :: [([Char],Int)] -> [([Char],Float)] -> [([Char],Float)]
aplicarOferta stock [] = []
aplicarOferta stock ((produc,price):xs) | stockDeProducto stock produc > 10 = (produc , price * 0.8) : aplicarOferta stock xs
                                        | otherwise = (produc , price) : aplicarOferta stock xs
