data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))

nuevo :: Organizador
nuevo = MkO emptyM emptyM 

{-
O(1).
La operación emptyM devuelve una estructura vacía con un costo constante 
O(1).
La construcción de MkO también tiene un costo constante 
O(1) ya que simplemente inicializa estructuras vacías.
-}

agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador 
agregarPrograma (MkO progAPers perAProg) prog personas = 

todosLosProgramas :: Organizador -> Checksum
todosLosProgramas (MkO progAPers perAProg) = domM progAPers

{-
O(M), donde M es la cantidad de programas en el organizador.
Utiliza domM para obtener las claves del mapa progAPers, que tiene un costo 
O(M) siendo M la cantidad de programas.
-}

autoresDe :: Organizador -> Checksum -> Set Persona
autoresDe (MkO progAPers perAProg) prog = case lookupM prog progAPers of 
                                            Just personas -> personas
                                            Nothing -> error"programa no encontrado"

{-
O(logM), donde M es la cantidad de programas en el organizador.
Utiliza lookupM para buscar un programa en el mapa progAPers, que tiene un costo 
O(logM) siendo M la cantidad de programas.
-}

programasDe :: Organizador -> Persona -> Set Checksum
programasDe (MkO progAPers perAProg) p = case lookupM p perAProg of 
                                            Just programas -> programas 
                                            Nothing -> error"persona no encontrada"

{-
O(logN), donde N es la cantidad de personas en el organizador.
Utiliza lookupM para buscar una persona en el mapa perAProg, que tiene un costo 
O(logN) siendo N la cantidad de personas.
-}

programaronJuntas :: Organizador -> Persona -> Persona -> Bool
programaronJuntas (MkO progAPers perAProg) p1 p2 = 



--

--implementar

nuevo :: Organizador 
nuevo = MkO emptyM emptyM

{-
O(1).
emptyM tiene un costo constante O(1)
-}

--
agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador 
agregarPrograma (MkO progAPers persAProg) prog p =  (MkO (asscoM prog p progAPers) -- (persAProg)  
--



todosLosProgramas :: Organizador -> [Checksum]
todosLosProgramas (MkO progAPers persAProg) = domM progAPers

{-
O(M), donde M es la cantidad de programas.
domM tiene un costo O(M) en un peor caso, ya que implica obtener las claves (checksums) del mapa de programas
-}

autoresDe :: Organizador -> Checksum -> Set Persona 
autoresDe (MkO progAPers persAProg) prog = case lookupM prog progAPers of
                                            Just autores -> autores
                                            Nothing -> emptyS 

{-
O(logM), donde M es la cantidad de programas.
lookupM tiene un costo O(logM) en un peor caso, ya que implica buscar un programa en el mapa de programas.
-}

programasDe :: Organizador -> Persona -> Set Checksum 
programasDe (MkO progAPers persAProg) p = case lookupM p persAProg of
                                            Just programas -> programas  
                                            Nothing -> emptyS  

{-
O(logM), donde M es la cantidad de programas.
lookupM tiene un costo O(logM) en un peor caso, ya que implica buscar una persona en el mapa de programas.
-}

programaronJuntas :: Organizador -> Persona -> Persona -> Bool 
programaronJuntas (MkO progAPers persAProg) p1 p2 = let programasDeP1 = lookupM p1 persAProg 
                                                        programasDeP2 = lookupM p2 persAProg 
                                                            in not isEmptyS (intersectionS (programasDeP1) (programasDeP2)) 


{-
O(logM), donde 
M es la cantidad de programas.
intersectionS tiene un costo O(min(logM,logM))=O(logM) en un peor caso.
-}

nroProgramasDePersona :: Organizador -> Persona -> Int 
nroProgramasDePersona (MkO progAPers persAProg) p = case lookupM p persAProg of 
                                                    Just programas -> sizeS programas 
                                                    Nothing -> error "persona no encontrada"

{-
O(logM), donde M es la cantidad de programas.
lookupM tiene un costo O(logM) en un peor caso, ya que implica buscar una persona en el mapa de programas.
-}

--usuario

programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum 
programasEnComun p1 p2 org = intersectionS (programasDe org p1) (programasDe org p2)

{-
O(logM), donde M es la cantidad de programas.
intersectionS tiene un costo O(min(logM,logM))=O(logM) en un peor caso.
-}

esUnGranHacker :: Organizador -> Persona -> Bool 
esUnGranHacker org p = length(todosLosProgramas org) == nroProgramasDePersona p org

{-
O(M), donde M es la cantidad de programas.
length tiene un costo O(M) en un peor caso al obtener la cantidad de programas.
nroProgramasDePersona tiene un costo O(logM)
-}






data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible 

--tads:
    --sector. tiene componentes y tripulantes
    --tripulante. tiene nombre, rango y sectores
    --sectorId (unico)
    --nombre y rango son strings. todos los tripulantes poseen un nombre unico.
    --sector vacio si no tiene tripulantes. nave vacia si no tiene tripulantes.
    --puede haber tripulantes sin sector asignado.


data Nave = N (Map sectorId sector) (Map Nombre Tripulante) (MaxHeap Tripulante)
                --k=sectorId v=sector   --rel un nombre con         t ordenados
                --rel un sector con      un tripulante (unico tmb)   por rango
                --su respectivo nro




--INVARIANTES:  
{-


-}



--implementacion

--b)
construir :: [SectorId] -> Nave
construir sectores = if null sectores
                        then (N emptyM emptyM emptyH)
                        else (N (crearMapSectores sectores) emptyM emptyH)


crearMapSectores :: [SectorId] -> Map SectorId Sector 
crearMapSectores [] = 
crearMapSectores (s:resto) = (crearS s)  (crearMapSectores resto)

                        




crearMapSectores :: [SectorId] -> Map SectorId Sector
crearMapSectores [] = emptyM
crearMapSectores (s:resto) = assocM s (crearS s) (crearMapSectores resto)
                           -- asoccM --sectorId --sector                --map  sectorId     sector


--c)
ingresarT :: Nombre -> Rango -> Nave -> Nave 
ingresarT n r (N sect trip rang) = (N sect (ingresar n trip) (insertH r trip))

ingresar :: Nombre -> Map Nombre Tripulante -> Map Nombre Tripulante 
ingresar n map = case lookupM n map of 
                    Just n -> map
                    Nothing -> assocM n emptyM map 

{-
O(log M), donde M es la cantidad de tripulantes.
  - `ingresar` tiene un costo O(log M) al insertar o actualizar un tripulante en el mapa.
  - `insertH` tiene un costo O(log T), donde T es el número de tripulantes en el heap.
-}


--d)
sectoresAsignados :: Nombre -> Nave -> Set SectorId
sectoresAsignados n (N sect trip rang) = sectoresT n 

{-
O(M log N), donde M es la cantidad de sectores y N es la cantidad de tripulantes.
  - `sectoresT` tiene un costo O(M log N) al buscar los sectores asignados a un tripulante.
-}

--e)
datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
datosDeSector s (N sect trip rang) = case lookupM s sect of
                                        Just sector -> (tripulantesS sector, componentesS sector)
                                        Nothing -> (emptyS, [])

{-
O(log M), donde M es la cantidad de sectores.
  - `lookupM` tiene un costo O(log M) en un peor caso al buscar un sector por su id en el mapa de sectores.
-}


--f)
tripulantesN :: Nave -> [Tripulante]
tripulantesN (N sect trip rang) = heapToList rang 


heapToList :: Heap a -> [a]
heapToList heap = if isEmptyH
                    then [] 
                    else maxH heap : heapToList (deleteMaxH heap)

{-
O(M log M), donde M es la cantidad de tripulantes.
  - `heapToList` tiene un costo O(M log M) al transformar el heap de tripulantes a una lista ordenada por rango.
-}

--g)
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector comps s (N sect trip rang) = case lookupM s sect of 
                                                Just sector -> (N sect trip rang) --Just _ -> N (assocM s (agregarCs comps (fromJust (lookupM s sect))) sect)
                                                Nothing -> (N (assocM s (agregarCs comps (crearS s))sect)   trip rang)
                                                 
agregarCs :: [Componente] -> Sector -> Sector 
agregarCs [] s = s 
agregarCs (x:xs) s = agregarCs xs (agregarC x s)

{-
O(log M), donde M es la cantidad de sectores.
  - `lookupM` tiene un costo O(log M) en un peor caso al buscar un sector por su id en el mapa de sectores.
  - `agregarCs` tiene un costo O(L), donde L es la cantidad de componentes a agregar.
-}

--h)
asignarASector :: Nombre -> SectorId -> Nave -> Nave
asignarASector n s (N sect trip rang) = case lookupM n trip of 
                                            Just nombre -> (N sect trip rang)
                                            Nothing -> (N (assocM s (crear s)) (asignar n trip) rang)  

asignar :: Nombre -> [Tripulante] -> Map Nombre Tripulante 
asignar _ [] = emptyM 
asignar n (t:ts) = if nombre t == n
                        then assocM n t (asignar n ts)
                        else asignar n ts 
{-
 O(log M), donde M es la cantidad de sectores.
  - `lookupM` tiene un costo O(log M) en un peor caso al buscar un tripulante por su nombre en el mapa de tripulantes.
  - `asignar` tiene un costo O(log M) al insertar o actualizar un tripulante en el mapa de tripulantes.
-}

--usuario

--i)
sectores :: Nave -> Set SectorId
sectores nave = primerMap nave

{-
O(M), donde M es la cantidad de sectores.
  - `primerMapa` tiene un costo O(1) al obtener el primer mapa de la nave.
  - `domM` tiene un costo O(M) al obtener el dominio del mapa de sectores.
-}

--
sinSectoresAsignados (N sect trip rang) = [t | t <- heapToList rang, not (asignado t trip)]

- Eficiencia justificada para `sinSectoresAsignados`: O(M log M), donde M es la cantidad de tripulantes.
  - `heapToList` tiene un costo O(M log M) al transformar el heap de tripulantes a una lista ordenada por rango.


--k)
barriles :: Nave -> [Barril]
barriles nave = barrilesC (componentesS (sectores nave))

barrilesC :: [Componente] -> [Barril]
barrilesC [] = []
barrilesC (x:xs) = if esBarril x 
                    then x : barrilesC xs
                    else barrilesC xs 

esBarril :: Componente -> Bool 
esBarril Almacen xs = if null xs 
                        then False
                        else True 
esBarril _ = False 

{-
O(M + L), donde M es la cantidad de sectores y L es la cantidad total de componentes en los sectores.
  - `mapaSectores` tiene un costo O(M) al obtener el mapa de sectores.
  - `concatMap` tiene un costo O(L) al concatenar las listas de componentes de los sectores.
-}



















--usuario

tripulantes :: Nave -> Set Tripulante 
tripulantes nave = tripulantesDeEn (sectores nave) nave 

tripulantesDeEn :: [Sector] -> Nave -> Set Tripulante 
tripulantesDeEn [] nave = emptyS
tripulantesDeEn (t:ts) nave = addS (t nave)  (tripulantesDeEn ts nave)

{-
--El costo total es O(n) + O(log n) , donde n es la cantidad de sectores de la nave. 
Esto se debe a que se itera sobre los sectores (O(n)), 
y dentro de cada iteración, se realiza una operación de búsqueda en un conjunto de tripulantes
 (O(log n)).
-}



--
data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)
--implementacion 

naveVacia :: [Sector] -> Nave 
naveVacia sectores = MkN emptyM emptyH (head sectores, 0)

{-
La construcción de una nave vacía con los sectores proporcionados tiene un costo constante 
O(1), ya que solo se inicializan estructuras de datos y se asignan valores iniciales.
-}


tripulantesDe :: Sector -> Nave -> Set Tripulante --costo: O(log s) por el lookupM, donde s son los sectores de secATrip.
tripulantesDe s (MkN secATrip trip sectors) = case lookupM s secATrip of 
                                                Just tripulantes -> tripulantes
                                                Nothing -> error"sector no encontrado"

{-
El costo de esta función es O(log s), donde s es la cantidad de sectores en la nave. 
Esto se debe a la operación de búsqueda en un mapa, que tiene una complejidad de 
O(log s) debido a lookupM.
-}


sectores :: Nave -> [Sector] --costo: O(n) donde n son los sectores del heap trip
sectores (MkN secATrip trip sectors) = domM secATrip 

{-
 Obtener los sectores de la nave tiene un costo O(n), ya que simplemente se obtienen las claves (sectores) de un mapa, lo cual tiene un costo lineal.
-}

conMayorRango :: Nave -> Tripulante --costo: O(1) donde agarro la raiz del heap trip. (osea el minimo elemento del heap, por eso es un heap, por como esta construido dicho arbol para que el minimo este disponible siendo lineal esa funcion).
conMayorRango (MkN secATrip trip sectors) = findMin trip 

{-
El costo de esta función es O(1) ya que simplemente se obtiene el mínimo de un heap, lo cual se logra en tiempo constante.
-}

conMasTripulantes :: Nave -> Sector --O(1) siendo fst sector el primer elemento del set sectores, teniendo en cuenta que se ordenan de mayor a menor en cuanto a los tripulantes de cada uno de los sectores de dicho set.
conMasTripulantes (MkN secATrip trip sectors) = fst sector 

{-
La obtención del sector con más tripulantes tiene un costo 
O(1) ya que se toma el primer elemento de una lista de sectores que ya está ordenada por la cantidad de tripulantes en cada sector.
-}

conRango :: Rango -> Nave -> Set Tripulante
conRango r (MkN secATrip tripuls sectores) = case lookupM r secATrip of
                                                Just tripulante -> tripulante 
                                                Nothing -> emptyS

{-
O(logN), donde N es el número de sectores en la nave.
lookupM tiene un costo O(logN) en un peor caso al buscar un rango en el mapa de sectores.
-}

sectorDe t (MkN secATrip tripuls sectores) = 
    case lookupM t secATrip of 
        Just s -> s
        Nothing -> error "Tripulante no encontrado"

{-
O(logN), donde N es el número de sectores en la nave.
lookupM tiene un costo O(logN) en un peor caso al buscar un tripulante en el mapa de sectores.
-}

agregarTripulante :: Tripulante -> Sector -> Nave -> Nave 
agregarTripulante t s (MkN secATrip tripuls sectores) = MkN (assocM s t secATrip) (insertH t tripuls) (addS s sectores)

{-
O(logN), donde N es el número de sectores en la nave.
assocM tiene un costo O(logN) en un peor caso al asociar un tripulante con un sector.
insertH tiene un costo O(logT), donde T es el número de tripulantes en el heap.
addS tiene un costo O(logN) al agregar un sector al conjunto.
-}


--usuario


tripulantes :: Nave -> Set Tripulante
tripulantes nave = tripulantesDeEn (sectores nave) nave 
                                    --[sector]

{-
O(N⋅logN), donde N es el número de sectores en la nave.
sectores tiene un costo O(N) al obtener la lista de sectores.
tripulantesDeEn tiene un costo O(N⋅logN) al agregar los tripulantes de cada sector a un conjunto. En cada llamada recursiva, se agrega un tripulante al conjunto, y hay 
N llamadas recursivas en total.
-}

tripulantesDeEn :: [Sector] -> Nave -> Set Tripulante 
tripulantesDeEn [] _ = emptyS
tripulantesDeEn (x:xs) nave = addS (x nave) (tripulantesDeEn xs nave)

{-
O(N⋅logN), donde N es el número de sectores en la nave.
En cada llamada recursiva, se agrega un tripulante al conjunto, y hay 

N llamadas recursivas en total.
addS tiene un costo O(logN) al agregar un tripulante al conjunto.
-}
























data EscuelaDeMagia = EDM (Set Hechizo) (Map Nombre Mago) (PriorityQueue Mago)

fundarEscuela :: EscuelaDeMagia 
fundarEscuela = EDM emptyS emptyM emptyPQ

{-
O(1).
La operación emptyS, emptyM y emptyPQ son O(1) ya que simplemente crean estructuras vacías con un costo constante.
La operación de construcción de EscuelaDeMagia es también 
O(1) ya que solo involucra inicializar estructuras vacías.
-}

estaVacia :: EscuelaDeMagia -> Bool 
estaVacia (EDM hs mgs rang) = isEmptyPQ rang 

{-
O(1).
Utiliza isEmptyPQ que es una operación de costo constante O(1) en una cola de prioridad.
-}

registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia 
registrar n (EDM hs mgs rang) = let newM = crearM n  
                                    in EDM hs (assocM n newM mags) (insertH newM rang)


{-
O(logM).
La operación insertH para insertar un mago en la cola de prioridad tiene un costo de 
O(logM) siendo M la cantidad de magos.
La operación assocM para agregar un mago en el mapa tiene un costo de 
O(logM) siendo M la cantidad de magos.
-}

magos :: EscuelaDeMagia -> [Nombre]
magos (EDM hs mgs rang) = domM mgs 

{-
Utiliza domM para obtener las claves del mapa, que tiene un costo 
O(M) siendo M la cantidad de magos.
-}

hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizos 
hechizosDe n (EDM hs mgs rang) = case lookupM n mgs of
                                    Just mago -> hechizos mago
                                    Nothing -> error"mago no encontrado"

{-
Utiliza lookupM para buscar un mago en el mapa, que tiene un costo 
O(logM) siendo M la cantidad de magos.
-}

leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int 
leFaltanAprender n (EDM hs mgs rang) = case lookupM n mgs of 
                                        Just mago -> sizeS hs - (sizeS hechizos mago)
                                        Nothing -> error"mago no existente"

{-
O(logM).
Utiliza lookupM para buscar un mago en el mapa, que tiene un costo 
O(logM) siendo 
M la cantidad de magos.
Operaciones como sizeS para obtener el tamaño de un conjunto son 
O(1), por lo que no afectan significativamente el costo en este contexto.
-}

egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
egresarUno (EDM sh mm pqm) = let m = maxPQ pqm
                                in (m, EDM sh (deleteM (nombre m) mm) (deleteMaxPQ pqm))

{-
O(logM), donde M es la cantidad de magos en la escuela.
maxPQ tiene un costo O(1) al obtener el máximo elemento de la cola de prioridad.
deleteM y deleteMaxPQ tienen un costo O(logM) en un peor caso, ya que implican eliminar un elemento en un mapa y en una cola de prioridad, respectivamente.
-}

enseñar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia
enseñar h n (EDM sh mm pqm) = case (lookupM n mm) of 
                                Nothing -> error "no es alumno"
                                Just m -> let newM = aprender h m 
                                    in EDM (addS h sh) (assocM n newM mm) (modificarPQ newM pqm)

modificarPQ :: Mago -> PriorityQueue Mago -> PriorityQueue Mago 
--prec: hay un mago con el mismo nombre que el mago dado.
modificarPQ n pqm = let MaxM = maxPQ pqm 
                        in if m == maxM 
                            then insertPQ m (deleteMaxPQ pqm)
                            else insertPQ maxM (modificarPQ m (deleteMaxPQ pqm))


{-
O(MlogM), donde M es la cantidad de magos en la escuela.
lookupM tiene un costo O(logM) en un peor caso.
aprender tiene un costo O(HlogH), donde 

H es la cantidad de hechizos aprendidos por el mago.
addS tiene un costo O(logH) en un peor caso.
assocM tiene un costo O(logM) en un peor caso al actualizar el mago en el mapa de magos.
modificarPQ tiene un costo 

O(MlogM) en un peor caso, ya que elimina y luego inserta en la cola de prioridad.
Estas funciones permiten egresar al mago más poderoso y enseñar un hechizo a un mago existente.
-}



--usuario




hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo 
--prop: retorna todos los hechizos aprendidos por los magos.
hechizosAprendidos escuela = hechizosDeEn (magos escuela) escuela

hechizosDeEn :: Mago -> EscuelaDeMagia -> Set Hechizo
hechizosDeEn [] _ = emptyS 
hechizosDeEn (n:ns) escuela = unionS (hechizosDe n escuela) (hechizosDeEn ns escuela)

{-
O(M×(logM+HlogH)), donde M es la cantidad de magos en la escuela y 
H es la cantidad de hechizos que la escuela ha enseñado.
Llamar a magos tiene un costo O(M).
hechizosDeEn llama M veces a hechizosDe, que tiene un costo O(logM).
hechizosDe tiene un costo O(logM) debido al uso de lookupM.
-}









hayUnExperto :: EscuelaDeMagia -> Bool 
hayUnExperto escuela = let m = fst (egresarUno escuela)
                                in leFaltaAprender (nombre m) escuela == 0

{-
O(logM), donde M es la cantidad de magos en la escuela.
Llamar a egresarUno tiene un costo O(logM)
-}


egresarExpertos :: EscuelaDeMagia -> ([Mago] , EscuelaDeMagia)
egresarExpertos escuela = if not (hayUnExperto escuela)
                            then ([], escuela)
                            else let (m, escuelaSinM) = egresarUno escuela 
                                     (ms, escuelaSinMs) = egresarExpertos escuelaSinM 
                                        in (m:ms, escuelaSinMs)

{-
O(MlogM), donde M es la cantidad de magos en la escuela.
Cada llamada a egresarUno tiene un costo O(logM).
Llamadas recursivas M veces.
Estas funciones permiten determinar los hechizos aprendidos por los magos, verificar si hay algún experto (que ha aprendido todos los hechizos) y egresar a todos los expertos junto con la escuela actualizada sin ellos.
-}














{-
Los invariantes de representación son condiciones que deben mantenerse verdaderas para garantizar que la estructura de datos cumple con su especificación y funcione correctamente. En el caso de la estructura dada, que es una RAList (Recursive Array List), los invariantes de representación son los siguientes:

Invariante de Representación de Heap (Cola de Prioridad):

La estructura heap es una cola de prioridad válida (heap). Es decir, cada elemento es menor o igual a sus hijos y se cumple la propiedad del heap.
Invariante de Representación de Map (Asociación de Posiciones con Elementos):

El mapa nAa debe contener asociaciones entre posiciones (enteros) y elementos (valores) en la estructura.
Invariante de Longitud de Map y Heap:

La longitud del mapa nAa debe ser igual a la longitud de la cola de prioridad heap.
Invariante de Tamaño Total de la Estructura:

El tamaño total n es igual al tamaño de la cola de prioridad heap y al tamaño del mapa nAa.
Estos invariantes aseguran que la estructura mantiene la consistencia y validez de los datos almacenados en la cola de prioridad y el mapa, así como la correcta relación entre el tamaño total y las estructuras individuales.
-}

data RAList a = MkR Int (Map Int a) (Heap a)
--              (MkR n nAa heap)

emptyRAL :: RAList a 
emptyRAL = (MkR 0 emptyM emptyH)

{-
O(1).
La operación emptyM devuelve una estructura vacía con un costo constante 
O(1).
La operación emptyH devuelve una cola de prioridad vacía con un costo constante 
O(1).
La construcción de MkR también tiene un costo constante 
O(1) ya que simplemente inicializa estructuras vacías y asigna el tamaño 
n=0.
-}

isEmptyRAL :: RAList a -> Bool 
isEmptyRAL (MkR n nAa heap) = isEmptyH heap 

{-
O(1).
Utiliza isEmptyH para verificar si la cola de prioridad (heap) está vacía, lo cual tiene un costo constante 
O(1).
-}

lenghtRAL :: RAList a -> Int 
lenghtRAL (MkR n nAa heap) = n

{-
O(1).
Devuelve el tamaño n almacenado en la estructura MkR, lo cual tiene un costo constante 
O(1).
-}

get :: Int -> RAList a -> a -- 
get n (MkR n nAa heap) = 

minRAL :: Ord a => RAList a -> a 
minRAL (MkR n nAa heap) = findMin heap

{-
O(1).
Utiliza findMin para encontrar el mínimo elemento en la cola de prioridad (heap), lo cual tiene un costo constante 
O(1) en una cola de prioridad.
-}

add :: Ord a => a -> RAList a -> RAList a --
add x (MkR n nAa heap) = MkR (n+1) (assocM n x nAa)  (insertH x heap)

{-
O(logN), donde N es el tamaño de la estructura antes de agregar el elemento.
La operación assocM para asociar x con la posición n en el mapa 
nAa tiene un costo O(logN).
La operación insertH para insertar x en la cola de prioridad tiene un costo 
O(logN).
-}

elems :: Ord a => RAList a -> [a]
elems (MkR n nAa heap) = heapToList heap

heapToList :: Ord a => Heap a -> [a]
heapToList heap
    | isEmptyH heap = []
    | otherwise = findMin heap : heapToList (deleteMin heap)

{-
O(NlogN), donde N es el tamaño de la estructura antes de extraer los elementos.
heapToList convierte la cola de prioridad (heap) en una lista, lo cual tiene un costo 
O(NlogN) ya que extraer cada elemento de una cola de prioridad tiene un costo 

O(logN).
-}

remove :: Ord a => RAList a -> RAList a 
remove (MkR n nAa heap) = MkR (n-1) (deleteM n-1 nAa) deleteMin heap  

{-
O(logN), donde N es el tamaño de la estructura antes de la eliminación.
La operación deleteMin elimina el mínimo elemento de la cola de prioridad (heap), lo cual tiene un costo 
O(logN) en una cola de prioridad.
-}

set :: Ord a => Int -> a -> RAList a -> RAList a
set i x (MkR n nAa heap) = MkR n (assocM i x nAa) (updateMin i x heap)

updateMin :: Ord a => Int -> a -> Heap a -> Heap a
updateMin i x heap = insertH x (deleteMin (dropH i heap))

{-
O(NlogN), donde N es el tamaño de la estructura antes de la modificación.
assocM para asociar x con la posición i en el mapa nAa tiene un costo 
O(logN).
deleteMin elimina el mínimo elemento de la cola de prioridad (heap), lo cual tiene un costo 
O(logN) en una cola de prioridad.
dropH elimina los primeros i elementos de la cola de prioridad, lo cual tiene un costo 
O(ilogN).
insertH inserta x en la cola de prioridad, lo cual tiene un costo O(logN).
-}





































































                                            
