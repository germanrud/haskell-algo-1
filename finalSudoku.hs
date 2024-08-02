
pertenece :: Int -> [Int] -> Bool
pertenece elem [] = False
pertenece elem (x:xs) = elem == x || pertenece elem xs


cuantasVecesAparece :: Int -> [Int] -> Int
cuantasVecesAparece elem [] = 0
cuantasVecesAparece elem (x:xs) | elem == x = 1 + cuantasVecesAparece elem xs
                                | otherwise = cuantasVecesAparece elem xs


hayRepetidosAux :: [Int] -> [Int] -> Bool
hayRepetidosAux [] lista = False
hayRepetidosAux (x:xs) lista = ((cuantasVecesAparece x lista) > 1 && x>0) || hayRepetidosAux xs lista


hayRepetidos :: [Int] -> Bool
hayRepetidos ls = hayRepetidosAux ls ls


algunaListaVacia :: [[Int]] -> Bool
algunaListaVacia [] = False
algunaListaVacia (ls:lss) = ls == [] || algunaListaVacia lss


obtenerColumna :: [[Int]] -> [Int]
obtenerColumna [] = []
obtenerColumna (ls:lss) | algunaListaVacia (ls:lss) = []
                        | otherwise = (head ls) : obtenerColumna lss 


sacarCabezaDeCadaFila :: [[Int]] -> [[Int]]
sacarCabezaDeCadaFila [] = []
sacarCabezaDeCadaFila (ls:lss) = tail ls : sacarCabezaDeCadaFila lss


trasponer :: [[Int]] -> [[Int]]
trasponer [] = []
trasponer lista | algunaListaVacia lista = []
                   | otherwise = obtenerColumna lista : trasponer (sacarCabezaDeCadaFila lista)



esMatrizValida :: [[Int]] -> Bool
esMatrizValida [] = True
esMatrizValida (ls:lss) = not (hayRepetidos ls) && esMatrizValida lss


esSudokuValido :: [[Int]] -> Bool
esSudokuValido m = esMatrizValida m && esMatrizValida (trasponer m)



