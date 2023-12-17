--1
heapSort :: Ord a => [a] -> [a]
{-
costo: Heapsort es un algoritmo de ordenación basado en la estructura de datos heap (montículo). 
La complejidad de heapsort depende de la forma en que se construye el heap y de cómo se extraen los elementos.

Suponiendo que utilizamos un heap para implementar la cola de prioridad y que esta implementación cumple con las propiedades de un heap (por ejemplo, un heap binario), la complejidad de heapsort sería:

Construcción del Heap (Heapify): O(n)

La construcción del heap desde una lista desordenada tiene un costo de O(n). Esta es la parte inicial de heapsort, donde se toma la lista y se organiza en una estructura de heap.

Extracción y Reconstrucción del Heap:

a. Extracción de elementos: O(n log n)

La extracción de elementos del heap se realiza n veces, y cada extracción tiene un costo logarítmico (O(log n)) en un heap.

b. Reconstrucción del heap: O(n log n)

Después de cada extracción, se requiere reconstruir el heap. La reconstrucción tiene un costo de O(log n) en cada extracción, y hay n extracciones, lo que suma O(n log n).

Dado que la construcción del heap (Heapify) domina la complejidad, la complejidad total de heapsort con esta implementación de heap es O(n) + O(n log n) = O(n log n).

En resumen, con una cola de prioridad implementada utilizando un heap con costos logarítmicos de inserción y borrado, la complejidad de heapsort es O(n log n).
-}
--2
belongsBST :: Ord a => a -> Tree a -> Bool 
belongsBST _ (EmptyT) = False
belongsBST x (NodeT y ti td) = if x==y then True
                                else if (x<y) 
                                    then belongsBST x ti
                                    else belongsBST x td 
                                     

insertBST :: Ord a => a -> Tree a -> Tree a 
insertBST x (EmptyT) = NodeT x EmptyT EmptyT
insertBST x (NodeT y ti td) = if x==y then  (NodeT x ti td)
                                else if (x<y)
                                    then NodeT (insertBST x ti) td
                                    else NodeT ti (insertBST x td) 

deleteBST :: Ord a => a -> Tree a -> Tree a 
deleteBST x (EmptyT) = EmptyT
deleteBST x (NodeT y ti td) = if x==y then rearmarBST ti td
                                else if (x<y)
                                    then NodeT y (deleteBST x ti) td 
                                    else NodeT y ti (deleteBST x td)

rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
--prec: los dos arboles son bst
rearmarBST izq der = let (m, izq') = splitMaxBST izq 
                        in NodeT m izq' der

splitMinBST :: Ord a => Tree a -> (a, Tree a)
splitMinBST (EmptyT) = error"el arbol esta vacio, no se puede dividir"
splitMinBST (NodeT y ti td) = (x,td)
splitMinBST (NodeT y ti td) = (minimo, NodeT y nuevoTi td)
    where
        (minimo, nuevoTi) = splitMinBST ti


splitMaxBST :: Ord a => Tree a -> (a, Tree a)
splitMaxBST (EmptyT) = error"el arbol esta vacio, no se puede dividir" 
splitMaxBST (NodeT y ti EmptyT) = (x,ti)
splitMaxBST (NodeT y ti td) = (maximo, NodeT y ti nuevoTd)
    where
        (maximo, nuevoTd) = splitMaxBST td

--------------
esBST :: Tree a -> Bool 
esBST (EmptyT) = False 
esBST (NodeT y ti td) = (todosMenoresQue x ti && todosMayoresQue x td) && esBST ti && esBST td

todosMayoresQue :: Ord a => a -> Tree a -> Bool
todosMayoresQue _ (EmptyT) = True
todosMayoresQue x (NodeT y ti td) = if x>y && todosMayoresQue x ti && todosMayoresQue x td

todosMenoresQue :: Ord a => a -> Tree a -> Bool
todosMenoresQue _ (EmptyT) = True
todosMenoresQue x (NodeT y ti td) = if x<y && todosMenoresQue x ti && todosMenoresQue x td
---------------

elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
elMaximoMenorA _ (EmptyT) = Nothing
elMaximoMenorA x (NodeT y ti td) = if y<x 
                                    then Just (max y (maxTi td))  -- Si y < x, hay un candidato en el nodo actual o en el subárbol derecho
                                    else elMaximoMenorA x ti -- Si y >= x, buscamos en el subárbol izquierdo

maxTi :: Ord a => Tree a -> a 
maxTi EmptyT = error"el arbol es vacio"
maxTi (NodeT y _ td) = max y (maxTi td) -- Comparamos el nodo actual con el máximo en el subárbol derecho
---------------

elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a 
elMinimoMayorA _ (EmptyT) = Nothing
elMinimoMayorA x (NodeT y ti td) = if y>x
                                    then Just (min y (minTd ti))
                                    else elMinimoMayorA x td

minTd :: Ord a => Tree a -> a 
minTd EmptyT = error"el arbol es vacio"
minTd (NodeT y ti _) = min y (minTd ti)

---------------

balanceado :: Tree a -> Bool 
balanceado (EmptyT) = True
balanceado (NodeT y ti td) = abs (altura ti - altura td) <= 1 && balanceado ti && balanceado td

altura :: Tree a -> Int 
altura (EmptyT) = 0
altura (NodeT y ti td) = 1 + max(altura ti) (altura td) 

abs :: Int -> Int
abs x = if x >= 0 then x else -x

--3

--Dada la siguiente interfaz y costos para el tipo abstracto Map:
{-

emptyM :: Map k v
Costo: O(1).

assocM :: Ord k => k -> v -> Map k v -> Map k v
Costo: O(log K).

lookupM :: Ord k => k -> Map k v -> Maybe v
Costo: O(log K).

deleteM :: Ord k => k -> Map k v -> Map k v
Costo: O(log K).

keys :: Map k v -> [k]
Costo: O(K)

-}

--Funciones como usuario de la practica anterior: 
{-

-}

--4
type SectorId = Int 
type CUIL = Int 

data Empresa = ConsE (Map SectorId (Set Empleado))
                     (Map CUIL Empleado)

{-

Donde se observa que:

-los empleados son un tipo abstracto.
-el primer map relaciona id de sectores con los empleados que trabajan en dicho sector.
-el segundo map relaciona empleados con su número de CUIL.
-un empleado puede estar asignado a más de un sector
-tanto Map como Set exponen una interfaz eficiente con costos logarítmicos para inserción,
búsqueda y borrado, tal cual vimos en clase.


Y sabemos que la interfaz de Empleado es:

consEmpleado :: CUIL -> Empleado
Propósito: construye un empleado con dicho CUIL.
Costo: O(1)

cuil :: Empleado -> CUIL
Propósito: indica el CUIL de un empleado.
Costo: O(1)

incorporarSector :: SectorId -> Empleado -> Empleado
Propósito: incorpora un sector al conjunto de sectores en los que trabaja un empleado.
Costo: O(log S), siendo S la cantidad de sectores que el empleado tiene asignados.

sectores :: Empleado -> SectorId
Propósito: indica los sectores en los que el empleado trabaja.
Costo: O(1)

Dicho esto, indicar invariantes de representación adecuados para la estructura y definir la
siguiente interfaz de Empresa, respetando los costos dados y calculando los faltantes. Justificar
todos los costos dados. En los costos, S es la cantidad de sectores de la empresa, y E es la
cantidad de empleados.

-}

--definir interfaz:

consEmpresa :: Empresa --O(1)
consEmpresa = ConsE emptyM emptyM

buscarPorCuil :: CUIL -> Empresa -> Empleado --O(log E)
buscarPorCuil cuil (ConsE _ mapCuilsDelEmpleado) = lookupM cuil mapCuilsDelEmpleado

empleadosDelSector :: SectorId -> Empresa -> [Empleado] --O(logS + E)
empleadosDelSector sector (ConsE mapDeLosSectores _) = case lookupM sector mapDeLosSectores
                                                        of 
                                                            Just setEmpleados = set2list setEmpleados    
                                                            Nothing = []                       


todosLosCUIL :: Empresa -> [CUIL] --O(E)
todosLosCUIL (ConsE _ mapCuilsDelEmpleado) = keys mapCuilsDelEmpleado

todosLosSectores :: Empresa -> [SectorId] --O(S)
todosLosSectores (ConsE mapDeLosSectores _) = keys mapDeLosSectores 


agregarSector :: SectorId -> Empresa -> Empresa --O(logS)
agregarSector nuevoSector (ConsE mapDeLosSectores _) = ConsE (insertM nuevoSector emptyS mapDeLosSectores) mapCuilsDelEmpleado 

agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa 
-- agrega un empleado a la empresa en el que trabajara en dichos sectores y tendra el cuil dado
-- costo: calcular

agregarASector :: SectorId -> CUIL -> Empresa -> Empresa 
-- agregar un sector al empleado con dicho CUIL
-- costo: calcular

borrarEmpleado :: CUIL -> Empresa -> Empresa 
-- elimina al empleado que posee dicho CUIL
-- costo: calcular
borrarEmpleado cuil (ConsE _ mapCuilsDelEmpleado) = 


--5
