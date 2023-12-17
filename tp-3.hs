data Color = Azul | Rojo
    deriving Show
data Celda = Bolita Color Celda | CeldaVacia
    deriving Show

--1.1

--1
nroBolitas :: Color -> Celda -> Int
nroBolitas c (CeldaVacia) = 0
nroBolitas c (Bolita color CeldaVacia) = unoSiCeroSino (esElMismoColor c color) + nroBolitas c CeldaVacia

esElMismoColor :: Color -> Color -> Bool
esElMismoColor Azul Azul = True
esElMismoColor Rojo Rojo = True
esElMismoColor _ _ = False 

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino False = 0

--2
poner :: Color -> Celda -> Celda
poner color celda = Bolita color celda

--3
sacar :: Color -> Celda -> Celda
sacar _ CeldaVacia = CeldaVacia  
sacar c (Bolita color celdaRestante) = if esElMismoColor c color 
                                            then celdaRestante  
                                            else Bolita color (sacar c celdaRestante)   

--4 coloque una subtarea que si hace recursion, para en la funcion principal no tener recursion
ponerN :: Int -> Color -> Celda -> Celda
ponerN n color celda = if n <= 0
                        then celda
                        else ponerNBolitas n color celda

ponerNBolitas :: Int -> Color -> Celda -> Celda
ponerNBolitas 0 _ celda = celda
ponerNBolitas n color celda = ponerNBolitas (n-1) color (Bolita color celda)

--1.2

data Objeto = Cacharro | Tesoro 
    deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show
--1
hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Cofre objetos restoDelCamino) = poseeTesoro objetos || hayTesoro restoDelCamino
hayTesoro (Nada restoDelCamino) = hayTesoro restoDelCamino

poseeTesoro :: [Objeto] -> Bool
poseeTesoro [] = False
poseeTesoro (x:xs) = esTesoro x || poseeTesoro xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False


--2
pasosHastaTesoro :: Camino -> Int
--debe existir al menos un tesoro.
pasosHastaTesoro Fin = error "Debe existir al menos un cofre con tesoro."
pasosHastaTesoro (Cofre objetos restoDelCamino) = if poseeTesoro objetos
                                     then 0 
                                     else 1 + pasosHastaTesoro restoDelCamino
pasosHastaTesoro (Nada caminoRestante) = 1 + pasosHastaTesoro caminoRestante


--3
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0 (Cofre objetos _) = poseeTesoro objetos 
hayTesoroEn 0 _ = False  
hayTesoroEn _ Fin = False  
hayTesoroEn n (Cofre _ restoDelCamino) = hayTesoroEn (n - 1) restoDelCamino  
hayTesoroEn n (Nada restoDelCamino) = hayTesoroEn (n - 1) restoDelCamino  



--4
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n _ = if n <= 0 then True else False 
alMenosNTesoros _ Fin = False 
alMenosNTesoros n (Cofre objetos restoDelCamino) =
                                                    poseeAlMenosNTesoros n objetos || alMenosNTesoros n restoDelCamino  
alMenosNTesoros n (Nada caminoRestante) =
                                            if n > 0
                                                then alMenosNTesoros n caminoRestante  
                                                else True  

poseeAlMenosNTesoros :: Int -> [Objeto] -> Bool
poseeAlMenosNTesoros _ [] = False
poseeAlMenosNTesoros n (obj:objs) =
                                     if esTesoro obj 
                                         then alMenosNTesoros (n-1) Fin 
                                         else poseeAlMenosNTesoros n objs



--5 desafio (lo intente pero no me sale pensarlo, lo entrego asi porque me dijeron que no es obligatorio)
--cantTesorosEntre :: Int -> Int -> Camino -> Int
--cantTesorosEntre n 
--cantTesorosEntre 0  
--cantTesorosEntre n    
--cantTesorosEntre 0 


-- 2.1

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

--1
sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT n izq der) = n + sumarT izq + sumarT der

--2
sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT _ izq der) = 1 + sizeT izq + sizeT der

--3
mapDoubleT :: Tree Int -> Tree Int
mapDoubleT EmptyT = EmptyT 
mapDoubleT (NodeT n izq der) = NodeT (multiplicarPorDos n) (mapDoubleT izq) (mapDoubleT der)

multiplicarPorDos :: Int -> Int
multiplicarPorDos n = n * 2

--4
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ EmptyT = False
perteneceT e (NodeT x izq der) = x == e || perteneceT e izq || perteneceT e der

--5
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ EmptyT = 0
aparicionesT e (NodeT x izq der) = (if e == x then 1 else 0) + aparicionesT e izq + aparicionesT e der 
                                    --aca quise usar unoSiCeroSino pero no me dejaba el compilador, asique puse el if


--6
leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT x EmptyT EmptyT) = [x] 
leaves (NodeT _ izq der) = leaves izq ++ leaves der 



--7
heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x izq der) = 1 + laMasAlta (heightT izq) (heightT der)

laMasAlta :: Int -> Int -> Int
laMasAlta t1 t2 = if t1 > t2 
                    then t1 
                    else t2

--8
mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT x izq der) = NodeT x (mirrorT der) (mirrorT izq)

--9
toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT x izq der) = toList izq ++ [x] ++ toList der

--10
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT = []
levelN n (NodeT x izq der) = if n == 0 
                                then [x] 
                                else levelN (n-1) izq ++ levelN (n-1) der


--11
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []  
listPerLevel (NodeT x izq der) = [x] : combinarLevels (listPerLevel izq) (listPerLevel der)

combinarLevels :: [[a]] -> [[a]] -> [[a]]
combinarLevels [] levels = levels
combinarLevels levels [] = levels
combinarLevels (l1:ls1) (l2:ls2) = (l1 ++ l2) : combinarLevels ls1 ls2

--12
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x izq der) = x : if longitud (ramaMasLarga izq) > longitud (ramaMasLarga der)
                                        then ramaMasLarga izq
                                        else ramaMasLarga der

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--13
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x EmptyT EmptyT) = [[x]]
todosLosCaminos (NodeT x izq der) = agregarElemento x (todosLosCaminos izq ++ todosLosCaminos der)

agregarElemento :: a -> [[a]] -> [[a]]
agregarElemento _ [] = []
agregarElemento y (xs:xss) = (y : xs) : agregarElemento y xss

--2.2

data ExpA = Valor Int | Sum ExpA ExpA 
                      | Prod ExpA ExpA 
                      | Neg ExpA
    deriving Show

--1
eval :: ExpA -> Int
eval (Valor n) = n
eval (Sum n1 n2) = eval n1 + eval n2 
eval (Prod n1 n2) = eval n1 * eval n2 
eval (Neg n) = - eval n


--2
simplificar :: ExpA -> ExpA
simplificar (Sum e1 e2) = simplificarSum (simplificar e1) (simplificar e2)
simplificar (Prod e1 e2) = simplificarProd (simplificar e1) (simplificar e2)
simplificar (Neg e) = simplificarNeg (simplificar e)
simplificar expr = expr  

-- Simplificar suma
simplificarSum :: ExpA -> ExpA -> ExpA
simplificarSum (Valor 0) expr = expr
simplificarSum expr (Valor 0) = expr
simplificarSum expr1 expr2 = Sum expr1 expr2

-- Simplificar producto
simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Valor 0) _ = Valor 0
simplificarProd _ (Valor 0) = Valor 0
simplificarProd (Valor 1) expr = expr
simplificarProd expr (Valor 1) = expr
simplificarProd expr1 expr2 = Prod expr1 expr2


-- Simplificar negación
simplificarNeg :: ExpA -> ExpA
simplificarNeg (Neg expr) = simplificarNegExpr (simplificar expr)
simplificarNeg expr = Neg expr

simplificarNegExpr :: ExpA -> ExpA
simplificarNegExpr (Neg expr) = expr 
simplificarNegExpr expr = Neg expr
