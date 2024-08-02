data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r*r
surface (Rectangle x1 y1 x2 y2) = (abs (x2 - x1)) * (abs (y2 - y1))  

data Person = Person {
    firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float
} deriving (Show)

--

type Punto2D = (Float,Float)

prodInt :: Punto2D -> Punto2D -> Float
prodInt (x1,y1) (x2,y2) = x1*x2 + y1*y2

type Coordenada = (Float,Float)

crearPar :: Float -> Float -> Coordenada
crearPar x y = (x,y)

--

type Ano = Int
type EsBisiesto = Bool

bisiesto :: Ano -> EsBisiesto
bisiesto n = (n `mod` 4 == 0) && ((n `mod` 100 /= 0) || (n `mod` 400 == 0))

--
type Texto = [Char]
type Nombre = Texto
type Telefono = Texto
type Contacto = (Nombre, Telefono)
type ContactosTel = [Contacto]

enLosContactos :: Nombre -> ContactosTel -> Bool
enLosContactos _ [] = False
enLosContactos n ((a,b):xs) = n == a || (enLosContactos n xs)
--

agregarContacto :: Contacto -> ContactosTel -> ContactosTel
agregarContacto (a,b) ((c,d):xs) | not (a `enLosContactos` ((c,d):xs)) = (a,b) : ((c,d):xs)
                                 | a == c = ((c,b):xs)
                                 | otherwise = (c,d):(agregarContacto (a,b) xs)
--

eliminarContacto :: Nombre -> ContactosTel -> ContactosTel
eliminarContacto name ((a,b):xs) | not (name `enLosContactos` ((a,b):xs)) = ((a,b):xs)
                                 | name == a = xs
                                 | otherwise = (a,b) : eliminarContacto name xs
--

type Identificacion = Integer
type Ubicacion = Texto
type Estado = (Disponibilidad, Ubicacion)
type Locker = (Identificacion, Estado)
type MapaDeLockers = [Locker]

data Disponibilidad = Libre | Ocupado deriving (Eq, Show)

existeElLocker ::  Identificacion -> MapaDeLockers -> Bool
existeElLocker n [] = False
existeElLocker n ((id,(disp,ubic)):xs) = n == id || existeElLocker n xs


ubicacionDelLocker :: Identificacion -> MapaDeLockers -> Ubicacion
ubicacionDelLocker n ((id,(disp,ubic)):xs) | n == id = ubic
                                           | otherwise = ubicacionDelLocker n xs

estaDisponibleElLocker :: Identificacion -> MapaDeLockers -> Bool
estaDisponibleElLocker n ((id,(disp,ubic)):xs) | n == id && disp == Libre = True
                                               | n == id && disp == Ocupado = False
                                               | otherwise = estaDisponibleElLocker n xs 

ocuparLocker :: Identificacion -> MapaDeLockers -> MapaDeLockers
ocuparLocker n ((id,(disp,ubic)):xs) | n == id = ((id,(Ocupado,ubic)):xs)
                                     | otherwise = (id,(disp,ubic)) : ocuparLocker n xs

