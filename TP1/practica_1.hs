--NUMEROS ENTEROS
--a)
sucesor :: Int -> Int
sucesor  a =  a + 1
--b)
sumar :: Int -> Int -> Int
sumar     a    b    =  a + b
--c)
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto   a  b    = (div a b ,mod a b )
--d)
maxDelPar :: (Int,Int) -> Int
maxDelPar  (a , b ) = if(a>b)
                            then a
                            else b

--1
-- divisionYResto (sumar 9 (maxDelPar (0 (sucesor 0))))
--2
-- maxDelPar(sumar( 5 divisionYResto(10 sucesor(1)))) 
--3 
-- sucesor(divisionYResto(27 maximoDelPar(1, sumar(2 1))))
--4
-- sumar(sucesor(0)maximoDelPar( divisionYResto(18 2)))




--TIPOS ENUMERATIVOS

--1)
data Dir = Norte | Este | Sur | Oeste deriving Show
--a)
opuesto :: Dir -> Dir
opuesto   Norte = Sur
opuesto   Sur = Norte
opuesto   Este = Oeste
opuesto   Oeste = Este
--b)
iguales :: Dir -> Dir -> Bool
iguales    Norte Norte = True
iguales    Sur  Sur   = True
iguales    Este Este  = True
iguales    Oeste Oeste = True
iguales    _    _     = False
--c)
siguiente :: Dir -> Dir
siguiente  Norte = Este
siguiente  Este  = Sur
siguiente  Sur = Oeste
siguiente  Oeste = Norte
--2)
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show

--a)
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)
--b)
empiezaConM :: DiaDeSemana -> Bool
empiezaConM    Martes  = True
empiezaConM    Miercoles = True
empiezaConM     _   = False
--c)
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues    Martes    Lunes   = True
vieneDespues    Miercoles Martes  = True
vieneDespues    Jueves  Miercoles = True
vieneDespues    Viernes Jueves    = True
vieneDespues    Sabado  Viernes   = True
vieneDespues    Domingo Sabado    = True
vieneDespues     _       _        = False
--d)
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio    Lunes       = False
estaEnElMedio    Domingo     = False
estaEnElMedio    _           =  True

--3

--a)
negar :: Bool -> Bool
negar  a   = not a

--b)
implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _= True
--c)
yTambien :: Bool -> Bool -> Bool
yTambien     a      b    =  a && b
--d)
oBien :: Bool -> Bool -> Bool
oBien     a      b    =  a || b

--REGISTROS
data Persona = P String Int 
                deriving Show

nombre :: Persona -> String
nombre (P n e)    = n   

edad :: Persona -> Int
edad     (P n e) = e

crecer :: Persona -> Persona
crecer    (P n e) = (P n (e+1))

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre    n1        (P n e) = (P n1 e)

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra    (P n1 e1)    (P n2 e2)  = e1 > e2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor    (P n1 e1)    (P n2 e2)  = if e1 > e2
                                             then (P n1 e1)
                                            else  (P n2 e2) 


  
juan :: Persona  
juan = P "juan" 24
maria :: Persona  
maria = P "maria" 42  


data TipoDePokemon = Agua | Fuego | Planta  deriving (Eq, Show)
data Pokemon = PK TipoDePokemon Int deriving Show
data Entrenador = E String Pokemon Pokemon deriving Show


entrenador1 :: Entrenador
entrenador1 = E "Nico" pok1 pok2
entrenador2 :: Entrenador
entrenador2 = E "Gus" pok2 pok3
entrenador3 :: Entrenador
entrenador3 = E "Isra" pok1 pok4

pok1 :: Pokemon 
pok1 = PK Agua 100 
pok2 :: Pokemon 
pok2 = PK Fuego 80
pok3 :: Pokemon 
pok3 = PK Planta 90 
pok4 :: Pokemon 
pok4 = PK Agua 70


tipoDePokemonEsSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
tipoDePokemonEsSuperior Agua Fuego = True
tipoDePokemonEsSuperior Fuego Planta = True
tipoDePokemonEsSuperior Planta Agua = True
tipoDePokemonEsSuperior _ _ = False

superaA :: Pokemon -> Pokemon -> Bool
superaA (PK tp ent) (PK tp1 ent1) = tipoDePokemonEsSuperior tp tp1

sonDelMismoTipo :: TipoDePokemon -> Pokemon -> Int
sonDelMismoTipo     tp              (PK t e1)   = if (tp == t)
                                                    then 1
                                                    else 0

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe     tp              (E _ pk1 pk2) = (sonDelMismoTipo tp pk1) + (sonDelMismoTipo tp pk2)

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon ((E _ pk1 pk2), (E _ pk3 pk4)) = pk1 : pk2 : pk3 : pk4 : []
juntarPokemon       _                      = []

--FUNCIONES POLIMORFICAS

loMismo :: a -> a
loMismo    a = a

siempreSiete :: a -> Int
siempreSiete    a = 7

swap :: (a,b) -> (b, a)
swap    (a,b) =  (b, a)

--PATTERN MATCHING SOBRE LISTAS

estaVacia :: [a] -> Bool
estaVacia    []  = True
estaVacia    _   = False

elPrimero :: [a] -> a
elPrimero   (x:xs) = x
-- la lista no tiene que estar vacia

splitHead :: [a] -> (a, [a])
splitHead  (x:xs) = (x, xs)
-- la lista no tiene que estar vacia