--1 COSTOS

head' :: [a] -> a
head' (x:xs) = x --constante

sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 --constante

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1) --lineal

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs --lineal

factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs --cuadratica

pertenece :: [Int] -> [Int]
pertenece n [] = False
pertenece (x:xs) = n == x || pertenece n xs --lineal

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs
                        then sinRepetidos xs  
                        else x : sinRepetidos xs --cuadratica    


append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) = x : append xs --lineal

concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs --lineal

takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) x : takeN (n-1) xs --lineal

dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs --lineal


partir :: Int -> [a] -> ([a],[a])
partir n xs = (takeN xs, dropN n xs) --lineal

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs) --lineal

sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) = if n == x
                    then xs
                    else x :  sacar n xs --lineal

ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar xs = let m
                = minimo xs
                in m : ordenar (sacar m xs) --cubica

















--2 SET


--2.1

emptyS :: Set a --O(1)
--Crea un conjunto vacío.
emptyS = ([] , 0) --lista y cantidad de elementos

addS :: Eq a => a -> Set a -> Set a --O(n)
--Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS x (xs,n) = if x `elem` xs
                    then (xs, n)
                    else (x : xs, n + 1)

belongs :: Eq a => a -> Set a -> Bool --O(n)
--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs x (xs, n) = x `elem` xs


sizeS :: Eq a => Set a -> Int --O(1)
--Devuelve la cantidad de elementos distintos de un conjunto.
sizeS (xs, n) = n

removeS :: Eq a => a -> Set a -> Set a --removeS es dificil
--Borra un elemento del conjunto.
removeS _ ([], n) = ([], n)
removeS x (xs, n) = if x == head xs
                        then (tail xs, n - 1)
                        else () -- else (head xs : fst(removeS x (tail xs, n)), n)

unionS :: Eq a => Set a -> Set a -> Set a
--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unionS ([], n1) (ys, n2) = (ys, n2)
union (xs, n1) ([], n2) = (xs, n1)
unionS (xs, n1) (ys, n2) = (xs ++ añadir ys xs, n1 + n2)

añadir :: Set a -> Set a
añadir [] _ = []  
añadir (y:ys') xs' = if y `elem` xs'
                        then añadir ys' xs'
                        else y : añadir ys' xs'

setToList :: Eq a => Set a -> [a]
--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList (xs, n) = xs


--2

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] set = []
losQuePertenecen (x:xs) set = if x belongs set
                                then x : losQuePertenecen xs (removeS e set)
                                else losQuePertenecen xs (removeS e set)


sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = setToList (addS x) ++ sinRepetidos xs


unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos (EmptyT) = emptyS
unirTodos (NodeT set izq der) = unionS set (unionS (unirTodos izq) (unirTodos der))


--3 implementar..












--3 QUEUE
--1
emptyQ :: Queue a
emptyQ = Queue []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Queue q) = null q

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue q) = Queue (q ++ [x])

firstQ :: Queue a -> a
firstQ (Queue q) = 
                    if null q
                        then error "La queue está vacía"
                        else head q

dequeue :: Queue a -> Queue a
dequeue (Queue q) =
                    if null q
                        then error "La queue está vacía"
                        else Queue (tail q)

--2 implementar..Implemente ahora la versión que agrega por delante y quita por el nal de la lista. Compare
--la eficiencia entre ambas implementaciones

--3

lenghtQ :: Queue a -> Int
lenghtQ = if isEmptyQ q
                then 0
                else 1 + lenght (dequeue q)  

queueToList :: Queue a -> [a]
queueToList = if isEmptyQ
                then []
                else firstQ q : queueToList (enqueue q)

unionQ :: Queue a -> Queue a -> Queue a
unionQ [] q2 = q2
unionQ q1 [] = q1
unionQ q1 q2 = (firstQ q1) (firstQ q2) unionQ (unionQ (enqueue q1) (enqueue q2)) -----












--4 STACK

emptyS :: Stack a
--Crea una pila vacía.
emptyS = Stack []

isEmptyS :: Stack a -> Bool
--Dada una pila indica si está vacía.
isEmptyS (Stack s) = null s 

push :: a -> Stack a -> Stack a
--Dados un elemento y una pila, agrega el elemento a la pila.
push x (Stack s)  = Stack (x:s)

top :: Stack a -> a
--Dada un pila devuelve el elemento del tope de la pila.
top (Stack s) = head s 

pop :: Stack a -> Stack a
--Dada una pila devuelve la pila sin el primer elemento.
pop (Stack s) = Stack (tail s)


lenS :: Stack a -> Int
--Dada la cantidad de elementos en la pila.
--Costo: constante.
lenS (Stack s) = lenght s

--1 usuario (la unica que queda


apilar :: [a] -> Stack a
apilar [] = emptyS
apilar (x:xs) = push x (apilar xs)

desapilar :: Stack a -> [a]
desapilar (Stack s) = if isEmptyS 
                        then 
                        else  

insertarEnPos :: Int -> a -> Stack a -> Stack a
insertarEnPos 












--5 QUEUE CON DOS LISTAS

emptyQ :: Queue a
emptyQ = (Q [] [])

isEmptyQ :: Queue a -> Bool
isEmptyQ (Q fs _) = null fs

enqueue :: a -> Queue a -> Queue a
enqueue x (Q fs bs) = if null fs
                        then Q (x: fs) bs
                        else Q fs (x:bs)

dequeue :: Queue a -> Queue a
--la lista fs no debe ser vacia
dequeue (Q fs bs) = if lenght fs == 1 
                        then Q (reverse bs) [] 
                        else Q (tail fs) bs

firstQ :: Queue a -> a
firstQ (Q fs _) = head fs


