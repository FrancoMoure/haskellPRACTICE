--this is gonna be written in spanish beacuse the university that I am attending is sittuated in Argentina.
--With this, I wanna prove that I have a basic knowledge of functional programming, even though I wanna get a job as a Junior Frontend Developer. Also, I can prove with this that I have mathematical skills and that I am fluent in spanish (which could be obvious beacuse I am from Argentina).
--every exercise name is gonna be translated into english so you can understand everything.

--1 (sucesor means "the next number")
sucesor :: Int -> Int 
sucesor x = x + 1

--2 (sumar means "add")
sumar :: Int -> Int -> Int
sumar x y = x + y

--3 (division y resto means "division and remainer")
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto a b = (a `div` b, a `mod` b)

--4 (max del par means "maximum of the pair")
maxDelPar :: (Int,Int) -> Int
maxDelPar (x,y) = if x > y 
                    then x 
                    else y

--5 (creating a "data" in order to make functions with directions. (North, South, East and West))
data Dir = Norte | Sur | Este | Oeste
	deriving Show

--opuesto means "oposite"
opuesto :: Dir -> Dir 
opuesto Norte = Sur --if North then South
opuesto Sur = Norte --if South then North
opuesto Este = Oeste --if East then West
opuesto Oeste = Este --if West then East

--iguales means "the same"
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True --if North and North then True, they are the same
iguales Sur Sur = True --South and South is True
iguales Este Este = True --East and East is True
iguales Oeste Oeste = True --West and West is True
iguales _ _ = False --otherwise, returns False


--siguiente means "next" 
siguiente :: Dir -> Dir
siguiente Norte = Este --if North then East beacuse the next direction of North is East 
siguiente Este = Sur --if East then South
siguiente Sur = Oeste --if South then West
siguiente Oeste = error "Oeste no tiene direccion siguiente" --else, returns "error" beacuse in this case West has no "next direction". (it weird but the exercise was written that way)


--6
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
	deriving Show

--Dia de la semana means "day of the week".
--Lunes means "Monday", Martes means "Tuesday", Miercoles means "Wednesday", Jueves means "Thursday", Viernes means "Friday", Sabado means "Saturday" and Domingo means "Sunday".

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana) --primero y ultimo dia means "first and last day"
primeroYUltimoDia = (Lunes, Domingo) --(Monday, Sunday)

empiezaConM ::  DiaDeSemana -> Bool --empieza con M means "starts with M"
empiezaConM Martes = True 
empiezaConM Miercoles = True
empiezaConM _ = False 



--This requires a sub-task

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool --viene despues means "comes next"
vieneDespues d1 d2 = numeroSegunDia d1 > numeroSegunDia d2

numeroSegunDia :: DiaDeSemana -> Int --numero de dia means "day number"
numeroSegunDia Lunes = 1
numeroSegunDia Martes = 2
numeroSegunDia Miercoles = 3
numeroSegunDia Jueves = 4
numeroSegunDia Viernes = 5
numeroSegunDia Sabado = 6
numeroSegunDia Domingo = 7


--this function right here recieves a day and returns a boolean based on knowing if the day is on one of the two extrems of the week or in the middle.
estaEnElMedio ::  DiaDeSemana -> Bool --esta en el medio means "is in the middle"
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True
--this is a good use of pattern matching and knowing how to write less code.


--7
negar :: Bool -> Bool --negar means "negation"
negar True = False
negar False = True

implica :: Bool -> Bool -> Bool --implica means "it implies"
implica True b = b
implica False _ = True

yTambien :: Bool -> Bool -> Bool -- &&
yTambien True True = True
yTambien _ _ = False


oBien :: Bool -> Bool -> Bool -- ||
oBien True _ = True
oBien _ y = y 	
