type Fila = [Int]
type Tablero = [Fila]
type Posicion = (Int,Int)
type Camino = [Posicion]

--

maximoDeFila :: Fila -> Int
maximoDeFila [x] = x
maximoDeFila (x:y:xs) | x>y = maximoDeFila (x:xs)
                      | otherwise = maximoDeFila (y:xs)


maximo :: Tablero -> Int
maximo [x] = maximoDeFila x
maximo (x:y:xs) | maximoDeFila x > maximoDeFila y = maximo (x:xs)
                | otherwise = maximo (y:xs)

--

aplanar :: Tablero -> [Int]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

cantidadApariciones :: [Int] -> Int -> Int
cantidadApariciones [] elem = 0
cantidadApariciones (x:xs) elem | x == elem = 1 + cantidadApariciones xs elem
                                | otherwise = cantidadApariciones xs elem


masRepetidoAux :: [Int] -> [Int] -> Int
masRepetidoAux [x] lista = x
masRepetidoAux (x:y:xs) lista | cantidadApariciones lista x > cantidadApariciones lista y = masRepetidoAux (x:xs) lista
                              | otherwise = masRepetidoAux (y:xs) lista

masRepetido :: Tablero -> Int
masRepetido tablero = masRepetidoAux (aplanar tablero) (aplanar tablero)

--

obtenerFilaAdecuadaAux :: Tablero -> (Int,Int) -> Int -> Fila
obtenerFilaAdecuadaAux (f:fs) (fila,col) i |  fila == i = f
                                           | otherwise = obtenerFilaAdecuadaAux fs (fila,col) (i+1)

obtenerFilaAdecuada :: Tablero -> (Int,Int) -> Fila
obtenerFilaAdecuada t coordenada = obtenerFilaAdecuadaAux t coordenada 1



obtenerNumeroAux :: Fila -> (Int,Int) -> Int -> Int
obtenerNumeroAux (x:xs) (fila,col) j | col == j = x
                                     | otherwise = obtenerNumeroAux xs (fila,col) (j+1)


obtenerNum :: Tablero -> (Int,Int) -> Int
obtenerNum tablero coordenadas = obtenerNumeroAux (obtenerFilaAdecuada tablero coordenadas) coordenadas 1


valoresDeCamino :: Tablero -> Camino -> [Int]
valoresDeCamino tablero [] = []
valoresDeCamino tablero (coordenada:xs) = obtenerNum tablero coordenada : valoresDeCamino tablero xs

--



 







