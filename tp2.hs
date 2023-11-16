--RECURSION SOBRE LISTAS

--PUNTO 1

--1
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns

--2
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--3
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (n:ns) = n + 1 : sucesores ns 

--4 
conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (b:bs) = b && conjuncion bs

--5
disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (b:bs) = b || disyuncion bs

--6 
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xss) = x ++ aplanar xss

--7
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs

--8 
apariciones :: Eq a => a -> [a] -> Int
--el dato debe ser Int
apariciones _ [] = 0
apariciones e (x:xs) = if e == x 
                        then 1 + apariciones e xs 
                        else 0 + apariciones e xs

--9
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ [] = []
losMenoresA e (n:ns) = if n < e then n : losMenoresA e ns
                                else losMenoresA e ns

--10
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ [] = [] 
lasDeLongitudMayorA e (x:xss) = if longitud x > e 
                                    then x : lasDeLongitudMayorA e xss
                                    else lasDeLongitudMayorA e xss

--11 
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e = [e]
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e


                                    
--12
agregar :: [a] -> [a] -> [a]
agregar [] ys = ys
agregar (x:xs) ys = x : agregar xs ys


--13
reversa :: [a] -> [a]
reversa xs = reversaAux xs []

reversaAux :: [a] -> [a] -> [a]
reversaAux [] acc = acc
reversaAux (x:xs) acc = reversaAux xs (x:acc)


--14
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] ys = ys
zipMaximos xs [] = xs
zipMaximos (x:xs) (y:ys) = max x y : zipMaximos xs ys
