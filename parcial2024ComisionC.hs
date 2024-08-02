
cuantasMateriasAprobo :: [Int] -> Int
cuantasMateriasAprobo [] = 0
cuantasMateriasAprobo (x:xs) | x >= 4 = 1 + cuantasMateriasAprobo xs
                             | otherwise = cuantasMateriasAprobo xs


aproboMasDeNMaterias :: [([Char],[Int])] -> [Char] -> Int -> Bool
aproboMasDeNMaterias ((student,notas):xs) alumno n | student == alumno = cuantasMateriasAprobo notas > n
                                                   | otherwise = aproboMasDeNMaterias xs alumno n

--

longitud :: [Int] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

promedio :: [Int] -> Float
promedio l = fromIntegral (sumatoria l) / fromIntegral (longitud l)


noAplazos :: [Int] -> Bool
noAplazos [] = True
noAplazos (x:xs) = x >= 4 && noAplazos xs


buenosAlumnos :: [([Char],[Int])] -> [[Char]]
buenosAlumnos [] = []
buenosAlumnos ((student,notas):xs) | (promedio notas >= 8) && (noAplazos notas) = student : buenosAlumnos xs
                                   | otherwise = buenosAlumnos xs

--

mejorPromedio :: [([Char],[Int])] -> [Char]
mejorPromedio [x] = fst x
mejorPromedio ((student,notas):(estud,notes):xs) | promedio notas >= promedio notes = mejorPromedio ((student,notas):xs)
                                                 | otherwise = mejorPromedio ((estud,notes):xs)


--

mejorPromedioNum :: [([Char],[Int])] -> Float
mejorPromedioNum [x] = promedio (snd x)
mejorPromedioNum ((student,notas):(estud,notes):xs) | promedio notas >= promedio notes = mejorPromedioNum ((student,notas):xs)
                                                 | otherwise = mejorPromedioNum ((estud,notes):xs)

promAlumno :: [([Char],[Int])] -> [Char] -> Float
promAlumno ((student,notas):xs) nombre | student == nombre = promedio notas
                                       | otherwise = promAlumno xs nombre

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece x [] = False
pertenece x (y:ys) = x == y || pertenece x ys



seGraduoConHonores :: [([Char],[Int])] -> Int -> [Char] -> Bool
seGraduoConHonores registro n alumno = aproboMasDeNMaterias registro alumno (n-1) 
                                      && pertenece alumno (buenosAlumnos registro)
                                      && (mejorPromedioNum registro - promAlumno registro alumno) < 1

--




