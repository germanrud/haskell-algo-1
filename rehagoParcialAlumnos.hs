
cuantasMateriasAprobo :: [Int] -> Int
cuantasMateriasAprobo [] = 0
cuantasMateriasAprobo (x:xs) | x>=4 = 1 + cuantasMateriasAprobo xs
                             | otherwise = cuantasMateriasAprobo xs


aproboMasDeNMaterias :: [([Char], [Int])] -> [Char] -> Int -> Bool
aproboMasDeNMaterias ((nombre,notas):xs) alumno n | nombre == alumno = cuantasMateriasAprobo notas > n
                                                  | otherwise = aproboMasDeNMaterias xs alumno n


--
suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs

promedio :: [Int] -> Float
promedio notas = fromIntegral (suma notas) / fromIntegral (length notas)


cuantosAplazos :: [Int] -> Int
cuantosAplazos [] = 0
cuantosAplazos (x:xs) | x<4 = 1 + cuantosAplazos xs
                      | otherwise = cuantosAplazos xs 


buenosAlumnos :: [([Char], [Int])] -> [[Char]]
buenosAlumnos [] = []
buenosAlumnos ((nombre,notas):xs) | (promedio notas) >= 8 && (cuantosAplazos notas == 0) = nombre : buenosAlumnos xs
                                  | otherwise = buenosAlumnos xs

--

mejorPromedio :: [([Char], [Int])] -> [Char]
mejorPromedio [(nombre,notas)] = nombre
mejorPromedio ((nombre,notas):(segNombre, segNotas):xs) | promedio notas >= promedio segNotas = mejorPromedio ((nombre,notas):xs)
                                                        | otherwise = mejorPromedio ((segNombre, segNotas):xs)


mejorPromedioNumerico :: [([Char], [Int])] -> Float
mejorPromedioNumerico [(nombre,notas)] = promedio notas
mejorPromedioNumerico ((nombre,notas):(segNombre, segNotas):xs) | promedio notas >= promedio segNotas = mejorPromedioNumerico ((nombre,notas):xs)
                                                                | otherwise = mejorPromedioNumerico ((segNombre, segNotas):xs)

--

pertenece :: (Eq t) => [t] -> t -> Bool
pertenece [] x = False
pertenece (y:ys) x = x == y || pertenece ys x



seGraduoConHonores :: [([Char], [Int])] -> Int -> [Char] -> Bool
seGraduoConHonores ((nombre,notas):xs) canMat alumno =
    aproboMasDeNMaterias ((nombre,notas):xs) alumno (canMat -1) && pertenece (buenosAlumnos ((nombre,notas):xs)) alumno && 
    (mejorPromedioNumerico ((nombre,notas):xs) - (promedio notas)) < 1


