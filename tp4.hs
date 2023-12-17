--PUNTO 1

data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving Show

data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
    deriving Show

--1
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa ing p) = 1 + cantidadDeCapas p

--2
armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (ing:ings) = Capa ing (armarPizza ings)

--3
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa Jamon p) = sacarJamon p
sacarJamon (Capa ing p) = Capa ing (sacarJamon p)

--4
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa ing p) = (noEsAgregado ing) && tieneSoloSalsaYQueso p

noEsAgregado :: Ingrediente -> Bool
noEsAgregado Salsa = True
noEsAgregado Queso = True
noEsAgregado _ = False

--5
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa (Aceitunas n) p) = Capa (Aceitunas (n*2)) (duplicarAceitunas p)
duplicarAceitunas (Capa ing p) = Capa ing (duplicarAceitunas p)


--6
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p,p) : cantCapasPorPizza ps



--PUNTO 2

data Dir = Izq | Der
    deriving Show

data Objeto = Tesoro | Chatarra
    deriving Show

data Cofre = Cofre [Objeto]
    deriving Show

data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa 
    deriving Show

--1
hayTesoro :: Mapa -> Bool
hayTesoro (Fin c) = tieneTesoro c
hayTesoro (Bifurcacion c m1 m2) = tieneTesoro c || hayTesoro m1 || hayTesoro m2


tieneTesoro :: Cofre -> Bool
tieneTesoro (Cofre []) = False
tieneTesoro (Cofre (obj:objs)) = esTesoro obj || tieneTesoro (Cofre objs)

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

--2
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] (Fin c) = tieneTesoro c
hayTesoroEn (Izq:ds) (Bifurcacion c m1 m2) = hayTesoroEn ds m1
hayTesoroEn (Der:ds) (Bifurcacion c m1 m2) = hayTesoroEn ds m2

--3
caminoAlTesoro :: Mapa -> [Dir]
--prec: existe un tesoro en el mapa y es unico.
caminoAlTesoro (Fin c) = []
caminoAlTesoro (Bifurcacion c m1 m2) = if tieneTesoro c then []
                                                        else Izq : caminoAlTesoro m1

--4
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin c) = []
caminoDeLaRamaMasLarga (Bifurcacion c m1 m2) = laMasLarga (caminoDeLaRamaMasLarga m1) (caminoDeLaRamaMasLarga m2)

laMasLarga :: [a] -> [a] -> [a]
laMasLarga l1 l2 = if longitud l1 > longitud l2 then l1 else l2

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--5
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c) = [objetosDelCofre c]
tesorosPorNivel (Bifurcacion c m1 m2) = objetosDelCofre c : tesorosPorNivel m1 ++ tesorosPorNivel m2

objetosDelCofre :: Cofre -> [Objeto]
objetosDelCofre (Cofre o) = o

--6
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin c) = [[]]
todosLosCaminos (Bifurcacion c m1 m2) = todosLosCaminos m1 ++ todosLosCaminos m2


--PUNTO 3

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    deriving Show

data Barril = Comida | Oxigeno | Torpedo | Combustible
    deriving Show

data Sector = S SectorId [Componente] [Tripulante]
    deriving Show

type SectorId = String 

type Tripulante = String 

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show


data Nave = N (Tree Sector)
    deriving Show

--1
sectores :: Nave -> [SectorId]
sectores (N EmptyT) = []
sectores (N (NodeT (S sectorId _ _) izq der)) = sectorId : (sectores (N izq) ++ sectores (N der))

--2
poderDePropulsion :: Nave -> Int
poderDePropulsion (N EmptyT) = 0
poderDePropulsion (N (NodeT (S sector componentes _) izq der)) =  sumatoriaPoder (componentes) + poderDePropulsion (N izq) + poderDePropulsion (N der)

sumatoriaPoder :: [Componente] -> Int
sumatoriaPoder [] = 0
sumatoriaPoder (Motor n:comps) = n + sumatoriaPoder comps
sumatoriaPoder (comp:comps) = sumatoriaPoder comps

--3
barriles :: Nave -> [Barril]
barriles (N EmptyT) = []
barriles (N (NodeT (S sector componentes tripulantes) izq der)) = barrilesDe (componentes) ++ barriles (N izq) ++ barriles (N der)

barrilesDe :: [Componente] -> [Barril]
barrilesDe [] = []
barrilesDe (Almacen barriles : comps)  = barriles ++ barrilesDe comps
barrilesDe (comp:comps) = barrilesDe comps

--4
agregarAlSector :: [Componente] -> SectorId -> Nave -> Nave
agregarAlSector _ _ (N EmptyT) = N EmptyT 
agregarAlSector componentes sectorId (N (NodeT sector izq der)) =
                                                                    N (NodeT (agregarComponentes componentes sectorId sector) izq der)

agregarComponentes :: [Componente] -> SectorId -> Sector -> Sector
agregarComponentes comps idBuscado (S id componentes tripulantes) =
                                                                      if id == idBuscado
                                                                        then S id (componentes ++ comps) tripulantes
                                                                        else S id componentes tripulantes



--5
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA _ [] nave = nave 
asignarTripulanteA t (x:xs) (N sector) =
                                          N (asignarTripulanteEnSectores t (x:xs) sector)

asignarTripulanteEnSectores :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteEnSectores _ _ EmptyT = EmptyT  
asignarTripulanteEnSectores t ids (NodeT sector izq der) =
                                                            if elem (sectorId sector) ids
                                                              then NodeT (asignarTripulanteAlSector t (sectorId sector) sector) (asignarTripulanteEnSectores t ids izq) (asignarTripulanteEnSectores t ids der)
                                                              else NodeT sector (asignarTripulanteEnSectores t ids izq) (asignarTripulanteEnSectores t ids der)

asignarTripulanteAlSector :: Tripulante -> SectorId -> Sector -> Sector
asignarTripulanteAlSector t id (S sectorId comps tripulantes) =
                                                                if sectorId == id
                                                                  then S sectorId comps (t : tripulantes)  
                                                                  else S sectorId comps tripulantes

sectorId :: Sector -> SectorId
sectorId (S id _ _) = id


--6

sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados t (N EmptyT) = []
sectoresAsignados t (N (NodeT sector izq der)) =
                                                if estaTripulanteEnSector t sector
                                                    then [idDelSector sector] ++ sectoresAsignados t (N izq) ++ sectoresAsignados t (N der)
                                                    else sectoresAsignados t (N izq) ++ sectoresAsignados t (N der)

idDelSector :: Sector -> SectorId
idDelSector (S sectorId _ _) = sectorId

estaTripulanteEnSector :: Tripulante -> Sector -> Bool
estaTripulanteEnSector t (S _ _ tripulantes) = estaEnLista t tripulantes
estaTripulanteEnSector _ _ = False

estaEnLista :: Tripulante -> [Tripulante] -> Bool
estaEnLista _ [] = False
estaEnLista t (x:xs) = if x == t
                        then True
                        else estaEnLista t xs

--7
tripulantes :: Nave -> [Tripulante]
tripulantes (N EmptyT) = []
tripulantes (N (NodeT sector izq der)) = tripulantesEnSector sector ++ tripulantes (N izq) ++ tripulantes (N der)

tripulantesEnSector :: Sector -> [Tripulante]
tripulantesEnSector (S _ _ tripulantes) = tripulantes



--PUNTO 4

type Presa = String --nombre de la presa
 
type Territorio = String --nombre del territorio
 
type Nombre = String --nombre de lobo

data Lobo = 
    Cazador Nombre [Presa] Lobo Lobo Lobo 
                                                |
    Explorador Nombre [Territorio] Lobo Lobo 
                                                |
    Cria Nombre

        deriving Show

data Manada = M Lobo
    deriving Show


--1 no
--lobo1 = Cazador "Erick" ["Conejo"] (Cria "Fabian") (Cria "Ernesto") (Cria "George")
--lobo2 = Explorador "Robert" ["Bosque"] (Cria "Pepe") (Cria "Mbappe")
--lobo3 = Explorador "Salah" ["Anfield"] (Cria "Roberto") (Cria "John")


--lobos :: [Lobo]
--lobos = [lobo1, lobo2, lobo3]


--manada1 :: Manada
--manada1 = M lobos


--2
buenaCaza :: Manada -> Bool
buenaCaza (M Lobo) = 



--3
elAlfa :: Manada -> (Nombre, Int)
elAlfa (M Lobo) = 

--4
losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (M Lobo) = 


--5
exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M Lobo) = 


--6
superioresDelCazador :: Nombre -> Manada -> [Nombre]
superioresDelCazador n (M Lobo) = 



































{-

-- Función auxiliar para calcular la cantidad de presas cazadas por un lobo.
cantidadPresasCazadas :: Lobo -> Int
cantidadPresasCazadas (Cazador _ presas _ _ _) = length presas
cantidadPresasCazadas _ = 0

-- Función auxiliar para verificar si una manada tiene una buena caza.
buenaCazaLobo :: Lobo -> Bool
buenaCazaLobo (Cría _) = True
buenaCazaLobo lobo = cantidadPresasCazadas lobo >= restante
  where
    restante =
      case lobo of
        (Cazador _ _ l1 l2 l3) -> buenaCazaLobo l1 && buenaCazaLobo l2 && buenaCazaLobo l3
        (Explorador _ _ l1 l2) -> buenaCazaLobo l1 && buenaCazaLobo l2
        _ -> True

-- 3. elAlfa :: Manada -> (Nombre, Int)
elAlfa :: Manada -> (Nombre, Int)
elAlfa (M lobo) = elAlfaLobo lobo ("", 0)

-- Función auxiliar para encontrar el lobo con más presas cazadas.
elAlfaLobo :: Lobo -> (Nombre, Int) -> (Nombre, Int)
elAlfaLobo (Cazador nombre presas l1 l2 l3) acc =
  let cantPresas = length presas
  in if cantPresas > maxPresas
     then (nombre, cantPresas)
     else acc
  where
    (nombreMax, maxPresas) = acc
elAlfaLobo (Explorador _ _ l1 l2) acc = elAlfaLobo l1 (elAlfaLobo l2 acc)
elAlfaLobo _ acc = acc

-- 4. losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron _ (M (Cría _)) = []
losQueExploraron territorio (M (Explorador nombre territorios l1 l2)) =
  if elem territorio territorios
  then nombre : (losQueExploraron territorio (M l1) ++ losQueExploraron territorio (M l2))
  else losQueExploraron territorio (M l1) ++ losQueExploraron territorio (M l2)
losQueExploraron territorio (M lobo) = losQueExploraron territorio (M lobo)

-- 5. exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M lobo) = exploradoresPorTerritorioLobo lobo []

-- Función auxiliar para obtener la lista de territorios explorados por un lobo.
territoriosExploradosPorLobo :: Lobo -> [Territorio]
territoriosExploradosPorLobo (Explorador _ territorios _ _) = territorios
territoriosExploradosPorLobo (Cazador _ _ l1 l2 l3) =
  territoriosExploradosPorLobo l1 ++ territoriosExploradosPorLobo l2 ++ territoriosExploradosPorLobo l3
territoriosExploradosPorLobo _ = []

-- Función auxiliar para construir la lista de territorios explorados con los nombres de exploradores.
exploradoresPorTerritorioLobo :: Lobo -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
exploradoresPorTerritorioLobo (Explorador nombre territorios l1 l2) acc =
  let territoriosExplorados = territoriosExploradosPorLobo (Explorador nombre territorios l1 l2)
  in foldr (\territorio acc' -> if elem territorio (map fst acc') then acc' else (territorio, [nombre]) : acc') acc territoriosExplorados
exploradoresPorTerritorioLobo (Cazador _ _ l1 l2 l3) acc =
  let acc1 = exploradoresPorTerritorioLobo l1 acc
      acc2 = exploradoresPorTerritorioLobo l2 acc1
  in exploradoresPorTerritorioLobo l3 acc2
exploradoresPorTerritorioLobo _ acc = acc

-- 6. superioresDelCazador :: Nombre -> Manada -> [Nombre]
superioresDelCazador :: Nombre -> Manada -> [Nombre]
superioresDelCazador nombre (M lobo) = superioresDelCazadorLobo nombre lobo

-- Función auxiliar para encontrar los superiores de un cazador dado.
superioresDelCazadorLobo :: Nombre -> Lobo -> [Nombre]
superioresDelCazadorLobo nombre (Cazador n _ l1 l2 l3) =
  if n == nombre
  then [n]
  else concat [superioresDelCazadorLobo nombre l1, superioresDelCazadorLobo nombre l2, superioresDelCazadorLobo nombre l3]
superioresDelCazadorLobo nombre (Explorador n _ l1 l2) =
  if n == nombre
  then [n]
  else concat [superioresDelCazadorLobo nombre l1, superioresDelCazadorLobo nombre l2]
superioresDelCazadorLobo _ _ = []

-}
