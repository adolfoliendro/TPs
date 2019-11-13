type Circulo a = [a]

--Compara dos circulos elemento a elemento (sonListasIguales)
igualPosicion :: Circulo Integer -> Circulo Integer -> Bool
igualPosicion [] [] = True
igualPosicion (c:cx) (d:dy) = (c == d) && (igualPosicion cx dy)

--Usa la longitud de el primer circulo como variable para la recursividad
--Compara dos circulos por rotacion
igualRotada ::  Int -> Circulo Integer -> Circulo Integer -> Bool
igualRotada 0 (x:xs) (y:ys) = False
igualRotada n (x:xs) (y:ys) | igualPosicion (x:xs) (y:ys) = True
                            | otherwise = igualRotada (n-1) (x:xs) (ys++[y])

--Ej.1 del TP. Compara dos circulos por rotacion y longitud
sonCirculosIguales ::  Circulo Integer -> Circulo Integer -> Bool
sonCirculosIguales [] [] = True
sonCirculosIguales x y = (length x == length y)&&(igualRotada (length x) x y)

--Ej.4 del TP. A partir de una lista de circulos, revisa si el primer circulo
--está en otra posición
estaRepetidoPrimero :: Circulo [Integer] -> Bool
estaRepetidoPrimero [] = False
estaRepetidoPrimero [x] = False
estaRepetidoPrimero c = sonCirculosIguales (head c) (last c) || estaRepetidoPrimero (init c)

--Crea una lista de numeros naturales
crearLista :: Integer -> Circulo Integer
crearLista 0 = []
crearLista n = crearLista (n-1)++[n]

--Inserta en una lista x un numero n en la posicion p
insertarEn :: [Integer] -> Integer -> Integer -> [Integer]
insertarEn x n 0 = n:x
insertarEn x n p = (head x):(insertarEn (tail x) n (p-1))

--Inserta en una lista x un numero n en todas las posiciones, generando una lista de listas
--p debe tener el cero (Integer) como entrada
insertarEnTodos :: [Integer] -> Integer -> Integer -> [[Integer]]
insertarEnTodos x n p | p > l = []
                      | otherwise = insertarEn x n (l-p):(insertarEnTodos x n (p+1))
                      where l = fromIntegral(length x)
--insertarEnTodos :: [Integer] -> Integer -> [[Integer]]
--insertarEnTodos x n | length w > length x = []
--                    | otherwise w
--                    where w = insertarEn x n fromIntegral(length x)

--Inserta en cada lista de una lista de listas x un numero n en todas las posiciones
insertarEnTodasLasListas :: [[Integer]] -> Integer -> [[Integer]]
insertarEnTodasLasListas [] n = []
insertarEnTodasLasListas x n = (insertarEnTodos (head x) n 0)++(insertarEnTodasLasListas (tail x) n)

--Genera una lista de listas de todas las permutaciones de lo primeros n naturales
permutaciones :: Integer -> [[Integer]]
permutaciones 1 = [[1]]
permutaciones n = (insertarEnTodasLasListas (permutaciones (n-1)) n)
