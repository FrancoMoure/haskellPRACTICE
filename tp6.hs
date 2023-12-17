--1 PQ
--implementaciones:

emptyPQ :: PriorityQueue a --O(1)
emptyPQ = PQ []

isEmptyPQ :: PriorityQueue a -> Bool --O(1)
isEmptyPQ (PQ xs) = null xs

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a --O(1)
insertPQ x (PQ xs) = PQ (x:xs)  

findMinPQ :: Ord a => PriorityQueue a -> a --O(n)
findMinPQ (PQ xs) = minimum xs

deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a --O(n)
deleteMinPQ (PQ xs) = PQ (borrarMin xs)

borrarMin :: Ord a => [a] -> [a] --O(n)
--lista no vacia
borrarMin [x] = []
borrarMin (x:xs) = borrar (minimum xs) xs

borrar :: Eq a => a -> [a] -> [a] --O(n)
borrar x [] = []
borrar x (y:ys) = if x == y then ys else y : borrar x ys

--2 falta
heapSort :: Ord a => [a] -> [a]
heapSort [] = []
heapSort (x:xs) = sort x (heapSort xs)

sort :: a -> [a] -> a 
sort e [] = [e]
sort e (x:xs) = if e > x
                    then e : sort e xs
                    else sort e xs ++ [x]

--3 Interfaz de Map

emptyM :: Map k v --O(1)
emptyM = M []

assocM :: Eq k => k -> v -> Map k v -> Map k v --O(n)
assocM k v (M kvs) = M (asociar k v kvs)  

lookupM :: Eq k => k -> Map k v -> Maybe v --O(n)
lookupM k (M kvs) = buscar k kvs

deleteM :: Eq k => k -> Map k v -> Map k v --O(n)
deleteM k (M kvs) = borrar k kvs

keys :: Map k v -> [k] --O(n)
keys M (kvs) = claves kvs

--O(n)
buscar :: Eq k => k -> [(k,v)] -> Maybe v 
buscar k [] = Nothing
buscar k ((k',v'), kvs) = if k == k'
                            then Just v'
                            else buscar k kvs
--O(n)
claves :: [(k,v)] -> [k]
claves [] = []
claves ((k,v),kvs) = k : claves kvs  

--O(n)
asociar :: k -> v -> [(k,v)] -> [(k,v)]
asociar k v [] = [(k,v)]
asociar k v ((k',v'), kvs) = if k == k'
                                then (k',v) : kvs
                                else (k',v') : asociar k v kvs

--O(n)
borrar :: Eq k => k -> [(k,v)] -> [(k,v)]
borrar k [] = []
borrar k ((k',v') : kvs) = if k == k' then kvs else (k',v') : borrar k kvs


--usuario

valuesM :: Eq k => Map k v -> [Maybe v]
valuesM map = valoresDeEn map (keys map)

valoresDeEn :: Eq k => Map k v -> [k] -> [Maybe v]
valoresDeEn map [] = []
valoresDeEn map (k:ks) = lookupM k map : valoresDeEn map ks
--
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas [] map = True
todasAsociadas (k:ks) map = if isJust (lookupM k map)
                                then todasAsociadas ks map
                                else False                              

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

--o tambien:
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas [] map = True
todasAsociadas (k:ks) map = case lookupM k map of
                                Just vs -> todasAsociadas ks map
                                Nothing -> False  

--
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap [] = emptyM
listToMap ((k, v): kvs) = assocM k v (listToMap kvs)

--
mapToList :: Eq k => Map k v -> [(k, v)]
mapToList emptyM = [] 
mapToList map = if isEmptyM map
                    then []
                    else (firstKey, fromJust (lookupM firstKey map)) : mapToList (deleteM firstKey map)

--
agruparEq :: Eq k => [(k, v)] -> Map k [v]
agruparEq [] = emptyM
agruparEq ((k, v):kvs) = assocM k [v] (agruparEq kvs)

--
incrementar :: Eq k => [k] -> Map k Int -> Map k Int --no
incrementar [] map n = map n
incrementar (k:ks) map n = incrementar ks map n+1

--
mergeMaps :: Eq k => Map k v -> Map k v -> Map k v
mergeMaps map1 map2 = listToMap (mapToList map1 ++ mapToList map2)

--4
data Map k v = [k] [v] --dos listas
{-
inv.rep: ambas listas tienen la misma cantidad de elementos.  sin repetidos.
-}

emptyM :: Eq k => Map k v 
emptyM = M [] []

assocM :: Eq k => k -> v -> Map k v -> Map k v --con repetidos
assocM k v (M ks vs) = M (k:ks) (v:vs)

assocM :: Eq k => k -> v -> Map k v -> Map k v 
asscoM k v (M ks vs) = if elem k ks --si k esta en ks
                        then M ks (actualizarValores (pos k ks) v vs)
                        else M (k:ks) (v:vs)


actualizarValores :: Int -> a -> [a] -> [a]
actualizarValores _ x [] = error"lista vacia"
actualizarValores n x (y:ys) = if n==0
                                then n : ys
                                else y : actualizarValores (n-1) x ys

pos :: Eq a => a -> [a] -> Int
pos x [] = error"x no esta en la lista"
pos x (y:ys) = if x == y
                then 0 
                else 1 + pos x ys

--
lookupM :: Eq k => k -> Map k v -> Maybe v 
lookupM x (M ks vs) = if elem x ks
                        then Just (buscar (pos k ks) vs)
                        else Nothing

--buscar va a ser como actualizar pero busca algo --no
buscar :: Int -> a -> [a] -> [a]
buscar _ x [] = error"lista vacia"
buscar n x (y:ys) = if n==0
                                then n : ys
                                else y : buscar (n-1) x ys


deleteM :: Eq k => k -> Map k v -> Map k v --no
deleteM k M ks vs = if elem k ks
                        then M borrarValor (pos k ks) vs
                        else M

keys = ks --tengo q hacerla pero devuelvo ks porque no tiene repetidos


--5
indexar :: [a] -> Map Int a
indexar [] = 
indexar (x:xs) = indexar xs

-- 3. MultiSet (multiconjunto)
data MultiSet a = MS Map a Int

emptyMS :: MultiSet a
emptyMS = MS emptyM


addMS :: Ord a => a -> MultiSet a -> MultiSet a
addMS x (MS map) = case lookupM x map of --esto es como un elem
                    Just v -> MS assocM x (v+1) map
                    Nothing ->  MS assocM x 1 map


ocurrencesMS :: Ord a => a -> MultiSet a -> Int

           

unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a (opcional)
Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de
ambos multiconjuntos.

intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a (opcional)
intersectionMS (MS map1) (MS map2) = MS

multiSetToList :: MultiSet a -> [(a, Int)]
Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
su cantidad de ocurrencias.

1. Implementar el tipo abstracto MultiSet utilizando como representación un Map. Indicar los
ordenes de complejidad en peor caso de cada función de la interfaz, justicando las respuestas.
2. Reimplementar como usuario de MultiSet la función ocurrencias de ejercicios anteriores,
que dado un string cuenta la cantidad de ocurrencias de cada caracter en el string. En este
caso el resultado será un multiconjunto de caracteres.

