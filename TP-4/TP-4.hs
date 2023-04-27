--1. Pizzas

data Pizza = Prepizza
            | Capa Ingrediente Pizza deriving Show
data Ingrediente = Salsa
                    | Queso
                    | Jamon
                    | Aceitunas Int     deriving Show

pizza1 = (Capa Queso (Capa Salsa Prepizza))
pizza2 = (Capa Jamon (Capa Queso (Capa Salsa Prepizza)))
pizza3 = (Capa (Aceitunas 8) (Capa Jamon (Capa Queso (Capa Salsa Prepizza))))

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas    Prepizza  = 1
cantidadDeCapas   (Capa i p) = 1 + cantidadDeCapas p

{-Dada una pizza devuelve la cantidad de ingredientes-}

armarPizza :: [Ingrediente] -> Pizza
armarPizza      []          = Prepizza
armarPizza     (i:is)       = Capa i (armarPizza is)


{-Dada una lista de ingredientes construye una pizza -}
esJamon :: Ingrediente -> Bool
esJamon     Jamon        = True
esJamon     _            = False

sacarJamon :: Pizza -> Pizza
sacarJamon   (Prepizza) = Prepizza
sacarJamon   (Capa i p) = if esJamon i
                                then sacarJamon p
                                else Capa i (sacarJamon p)

{-Le saca los ingredientes que sean jamón a la pizza-}

esQueso :: Ingrediente -> Bool
esQueso     Queso        = True
esQueso     _            = False
esSalsa :: Ingrediente -> Bool
esSalsa     Salsa        = True
esSalsa     _            = False

tieneQueso :: Pizza -> Bool
tieneQueso   (Prepizza) = False
tieneQueso   (Capa i p) = esQueso i || tieneQueso p

tieneSalsa :: Pizza -> Bool
tieneSalsa   (Prepizza) = False
tieneSalsa   (Capa i p) = esSalsa i || tieneSalsa p

soloSalsaYQueso ::  Pizza -> Bool
soloSalsaYQueso   (Prepizza) = True
soloSalsaYQueso   (Capa i p) = ((esQueso i)||(esSalsa i)) && soloSalsaYQueso p


tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso p = tieneQueso p && tieneSalsa p && soloSalsaYQueso p
                                         
    

{-
Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En
particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero.)-}

dobleAceitunas :: Ingrediente -> Ingrediente
dobleAceitunas   (Aceitunas n)   = Aceitunas (n * 2) 
dobleAceitunas    i              = i

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas    Prepizza = Prepizza
duplicarAceitunas    (Capa i p) = Capa (dobleAceitunas i) (duplicarAceitunas p)

{-Recorre cada ingrediente y si es aceitunas duplica su cantidad-}

cantCapas :: Pizza -> Int
cantCapas   (Prepizza) = 0
cantCapas   (Capa i p) =  1 + cantCapas p 


cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza     []     = []
cantCapasPorPizza   (p:ps) = ((cantCapas p), p) : cantCapasPorPizza ps


{-Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
ingredientes de la pizza, y la respectiva pizza como segunda componente.-}


--2. Mapa de tesoros (con bifurcaciones)
{-Un mapa de tesoros es un árbol con bifurcaciones que terminan en cofres. Cada bifurcación y
cada cofre tiene un objeto, que puede ser chatarra o un tesoro.-}

data Dir = Izq | Der                      deriving Show
data Objeto = Tesoro | Chatarra           deriving Show
data Cofre = Cofre [Objeto]               deriving Show
data Mapa = Fin Cofre                     
            | Bifurcacion Cofre Mapa Mapa     deriving Show



--1. 
--arbol

arbMap :: Mapa
arbMap = Bifurcacion (Cofre [Chatarra])
                       (Bifurcacion (Cofre [Chatarra,Tesoro])
                                    (Bifurcacion (Cofre [Chatarra])
                                        (Bifurcacion (Cofre [Chatarra])
                                            (Fin (Cofre [Chatarra]))
                                            (Fin (Cofre [Chatarra]))
                                        )
                                        (Fin (Cofre [Tesoro]) )
                                    )

                                    (Bifurcacion (Cofre [Chatarra])
                                        (Fin(Cofre [Chatarra,Tesoro]))
                                        (Bifurcacion (Cofre [Tesoro,Tesoro])
                                            (Fin (Cofre [Chatarra]))
                                            (Fin(Cofre [Chatarra]))
                                        )
                                    )  
                        )

                       (Bifurcacion (Cofre [Chatarra])   
                                    (Bifurcacion (Cofre [Chatarra])
                                        (Fin (Cofre [Chatarra]))
                                        (Fin(Cofre [Chatarra]))
                                                                )

                                    (Fin(Cofre [Chatarra]))
                                    
                                    )


arbMap2 = Bifurcacion (Cofre [Chatarra])
                       (Bifurcacion (Cofre [Chatarra])
                                    (Bifurcacion (Cofre [Chatarra])
                                        (Fin(Cofre [Chatarra]))
                                        (Fin(Cofre [Chatarra,Tesoro]))
                                    )

                                    (Bifurcacion (Cofre [Chatarra])
                                        (Fin(Cofre [Chatarra]))
                                       (Fin(Cofre [Chatarra]))
                                    )  
                        )

                       (Bifurcacion (Cofre [Chatarra])   
                                    (Bifurcacion (Cofre [Chatarra])
                                        (Fin (Cofre [Chatarra]))
                                        (Fin(Cofre [Chatarra]))
                                                                )

                                    (Fin(Cofre [Chatarra]))
                                    
                                    ) 

esTesoro :: Objeto -> Bool
esTesoro   Tesoro = True
esTesoro   _      = False

cofreConTesoro :: [Objeto] -> Bool
cofreConTesoro   [] = False
cofreConTesoro   (t:ts) = esTesoro t ||cofreConTesoro ts

analizarCofre :: Cofre -> Bool
analizarCofre   (Cofre os) = cofreConTesoro os

hayTesoro :: Mapa -> Bool
hayTesoro    (Fin co)             = analizarCofre co
hayTesoro   (Bifurcacion co m1 m2) = (analizarCofre co) || hayTesoro m1 || hayTesoro m2

{-Indica si hay un tesoro en alguna parte del mapa.-}
--2. 
hayTesoroEnMapa :: Mapa -> Bool
hayTesoroEnMapa (Fin co) = analizarCofre co
hayTesoroEnMapa (Bifurcacion co ml mr) = analizarCofre co

esDirIzq :: Dir -> Bool
esDirIzq    Izq = True
esDirIzq    _  = False

irADir :: Dir -> Mapa -> Mapa
irADir   (d) (Bifurcacion co ml mr) = if esDirIzq d then ml else mr 
irADir   (d)  m                     = m

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn    []        m    = hayTesoroEnMapa m
hayTesoroEn    (d:ds)    m    = hayTesoroEn ds (irADir d m) 


{-Indica si al nal del camino hay un tesoro. Nota: el nal de un camino se representa con una
lista vacía de direcciones.-}

--3. 
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro   (Fin co) = []
caminoAlTesoro   (Bifurcacion co ml mr) = if hayTesoro ml
                                            then Izq : caminoAlTesoro ml
                                            else Der : caminoAlTesoro mr

{-Indica el camino al tesoro. Precondición: existe un tesoro y es único.-}

--4. 
heightR :: Mapa -> Int
heightR    (Fin co)  = 0
heightR    (Bifurcacion co ml mr)= 1 + max (heightR ml) (heightR mr)

esMasLargo :: Mapa -> Bool
esMasLargo   (Fin co) = False
esMasLargo   (Bifurcacion co ml mr) = if heightR ml > heightR mr 
                                             then True || esMasLargo ml
                                             else False || esMasLargo mr          

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga    (Fin co) =[]
caminoDeLaRamaMasLarga    (Bifurcacion co ml mr) = if esMasLargo ml 
                                                    then Izq : caminoDeLaRamaMasLarga ml
                                                    else Der : caminoDeLaRamaMasLarga mr

{-Indica el camino de la rama más larga.-}
--5. 
dameTesoro :: [Objeto] -> [Objeto]
dameTesoro     []      = []
dameTesoro     (o:os) = if esTesoro o then o : (dameTesoro os) else dameTesoro os

dameTesoroDelCofre :: Cofre -> [Objeto]
dameTesoroDelCofre   (Cofre os) =  dameTesoro os

hayTesoroEnLaLista :: [Objeto] -> Bool
hayTesoroEnLaLista   []    = False
hayTesoroEnLaLista   (o:os)= esTesoro o || hayTesoroEnLaLista os

hayTesoroEnElCofre :: Cofre -> Bool
hayTesoroEnElCofre   (Cofre os)=  hayTesoroEnLaLista os

juntarListas :: [[Objeto]] -> [[Objeto]] -> [[Objeto]]
juntarListas   []      os2      = os2
juntarListas   os1      []      = os1
juntarListas   (o1:os1) (o2:os2) = (o1 ++ o2) : juntarListas os1 os2

tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel    (Fin co) = [dameTesoroDelCofre co ]
tesorosPorNivel    (Bifurcacion co ml mr) =if hayTesoroEnElCofre co 
                                            then (dameTesoroDelCofre co ): (tesorosPorNivel ml ++ tesorosPorNivel mr)
                                            else juntarListas (tesorosPorNivel ml)  (tesorosPorNivel mr)

{-Devuelve los tesoros separados por nivel en el árbol.-}
--6.
aCadaDir :: Dir -> [[Dir]] -> [[Dir]]
aCadaDir    x    []       = []
aCadaDir    x   (xs:xss) = (x:xs) : aCadaDir x xss

todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos   (Fin co) = [[]]
todosLosCaminos   (Bifurcacion co ml mr) = ( aCadaDir Izq  (todosLosCaminos ml)) ++ (aCadaDir Der  (todosLosCaminos mr))

{-Devuelve todos lo caminos en el mapa.-}

--3. Nave Espacial-------------------------------------------------------------------------------
{-modelaremos una Nave como un tipo algebraico, el cual nos permite construir una nave espacial,
dividida en sectores, a los cuales podemos asignar tripulantes y componentes. La representación
es la siguiente:-}

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]  deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible  deriving Show
data Sector = S SectorId [Componente] [Tripulante] deriving Show
type SectorId = String
type Tripulante = String  
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show
data Nave = N (Tree Sector) deriving Show

-- Implementar las siguientes funciones utilizando recursión estructural:
nave1 :: Nave
nave1 = N 
            (NodeT  (S 
                        "sector1" 
                        [LanzaTorpedos, (Motor 6), (Almacen 
                                                [Comida, Oxigeno, Torpedo,Combustible]
                                                )
                        ]
                        ["Rick", "Max"]
                    )
                    (NodeT  (S 
                                "sector2" 
                                [LanzaTorpedos, (Motor 4), (Almacen 
                                                                [Comida, Oxigeno]
                                                            )
                                ]
                                [ "Roy", "Global"]
            )
                            EmptyT 
                            EmptyT
                    )  
                    EmptyT
            ) 
            
        
--1. 


dameElId :: Sector -> SectorId
dameElId   (S id cs ts) = id

buscarSectores :: Tree Sector -> [SectorId]
buscarSectores    EmptyT = []
buscarSectores    (NodeT a tl tr) = dameElId a : (buscarSectores tl ++ buscarSectores tr)

sectores :: Nave -> [SectorId]
sectores    (N a ) = buscarSectores a


{-Propósito: Devuelve todos los sectores de la nave.-}
--2. 
damePoder :: Componente -> Int
damePoder    (Motor n)  = n  

esMotor :: Componente -> Bool
esMotor    (Motor n) = True
esMotor    _     = False

siHayMotorDamePoder :: [Componente] -> Int
siHayMotorDamePoder    [] = 0
siHayMotorDamePoder (c:cs) = if esMotor c  then (damePoder c) + (siHayMotorDamePoder cs) else 0+ (siHayMotorDamePoder cs)

dameElMotor ::Sector ->Int
dameElMotor   (S id cs ts) = siHayMotorDamePoder cs

buscarPropulsion :: Tree Sector -> Int
buscarPropulsion    EmptyT = 0
buscarPropulsion    (NodeT s tl tr) = dameElMotor s + buscarPropulsion tl + buscarPropulsion tr

poderDePropulsion :: Nave -> Int
poderDePropulsion   (N a) = buscarPropulsion a

--Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
{-el poder de propulsión es el número que acompaña al constructor de motores.-}
--3. 
dameBarriles :: Componente -> [Barril]
dameBarriles    (Almacen bs) = bs


esAlmacen :: Componente -> Bool
esAlmacen    (Almacen bs) = True
esAlmacen    _           = False

barrilesDelComponente :: [Componente] -> [Barril]
barrilesDelComponente    []           = []
barrilesDelComponente    (c:cs)       = if esAlmacen c 
                                            then dameBarriles c ++ ( barrilesDelComponente cs)
                                            else barrilesDelComponente cs

barrilesDelSector :: Sector -> [Barril]
barrilesDelSector    (S id cs ts) =  barrilesDelComponente cs


barrilesDelArbol :: Tree Sector -> [Barril]
barrilesDelArbol    EmptyT = []
barrilesDelArbol (NodeT s tl tr) = barrilesDelSector s ++ (barrilesDelArbol tl ++ barrilesDelArbol tr)

barriles :: Nave -> [Barril]
barriles    (N a) = barrilesDelArbol a


--Propósito: Devuelve todos los barriles de la nave.
--4. 
agregarComp ::  [Componente] -> [Componente] ->  [Componente] 
agregarComp     c1               c2           =  c1 ++ c2

agregarCompASector :: [Componente] -> SectorId -> Sector -> Sector
agregarCompASector    cs             id        (S sid scs ts) = if id == sid
                                                                   then (S sid (agregarComp cs scs) ts)  
                                                                   else (S sid scs ts)                    

agregarComponentes ::[Componente] -> SectorId -> Tree Sector -> Tree Sector 
agregarComponentes    cs             id           EmptyT         = EmptyT
agregarComponentes    cs             id          (NodeT s tl tr) =(NodeT (agregarCompASector  cs id s ) (agregarComponentes cs id tl)(agregarComponentes cs id tr))

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector     cs             id          (N a) = N  (agregarComponentes cs id a)

--Propósito: Añade una lista de componentes a un sector de la nave.
--Nota: ese sector puede no existir, en cuyo caso no añade componentes.
--5.   
contieneId :: SectorId -> [SectorId] -> Bool
contieneId    (id)          []        = False
contieneId    (id)          (i:is)  = id == i || contieneId id is

asignarTripulanteAlSector :: Tripulante -> [SectorId] -> Sector -> Sector
asignarTripulanteAlSector    t             ids           (S sid scs ts) = if contieneId sid ids
                                                                            then (S sid scs (t :ts)) 
                                                                            else (S sid scs ts)

asignarTripulanteAlArbol ::  Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteAlArbol      t             ids          EmptyT          = EmptyT
asignarTripulanteAlArbol      t             ids          (NodeT s tl tr) = (NodeT (asignarTripulanteAlSector t ids s) (asignarTripulanteAlArbol t ids tl) (asignarTripulanteAlArbol t ids tr))
 
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA    t             ids          (N a) = N (asignarTripulanteAlArbol t ids a)

--Propósito: Incorpora un tripulante a una lista de sectores de la nave.
--Precondición: Todos los id de la lista existen en la nave.

hayTripulante :: Tripulante -> [Tripulante] -> Bool
hayTripulante    t             []             =False
hayTripulante    (t)          (i:is)  = t == i || hayTripulante t is

sectoresDelTripulante :: Tripulante -> Sector -> [SectorId]
sectoresDelTripulante    t            (S id scs ts) = if  hayTripulante t ts then [id] else []

sectoresDelArbol :: Tripulante -> Tree Sector -> [SectorId]
sectoresDelArbol    t             EmptyT          = []
sectoresDelArbol    t             (NodeT s tl tr) = (sectoresDelTripulante t s) ++ (sectoresDelArbol t tl) ++ (sectoresDelArbol t tr)

sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados    t              (N a) = sectoresDelArbol t a

--Propósito: Devuelve los sectores en donde aparece un tripulante dado.

--7.
estaIncluido :: Tripulante-> [Tripulante]->Bool
estaIncluido    t              []             = False
estaIncluido    (t)            (i:is)  = t == i || estaIncluido t is

sinTripulantesRepetidos :: [Tripulante]-> [Tripulante]-> [Tripulante]
sinTripulantesRepetidos    t1            []           =  t1
sinTripulantesRepetidos    []            t2           =  t2   
sinTripulantesRepetidos    (t:ts)        t2           =  if estaIncluido t t2 
                                                            then  sinTripulantesRepetidos ts t2
                                                            else  t: sinTripulantesRepetidos ts t2

tripulantesDelSector :: Sector -> [Tripulante]
tripulantesDelSector    (S id scs ts) = ts

tripulantesDelArbol :: Tree Sector -> [Tripulante]
tripulantesDelArbol    EmptyT          = []
tripulantesDelArbol    (NodeT s tl tr) = (tripulantesDelSector s) ++ (tripulantesDelArbol tl) ++ (tripulantesDelArbol tr)               

tripulantes :: Nave -> [Tripulante]
tripulantes   (N a) = tripulantesDelArbol a

--Propósito: Devuelve la lista de tripulantes, sin elementos repetidos.

--4. Manada de lobos---------------------------------------------------------------------

{-Modelaremos una manada de lobos, como un tipo Manada, que es un simple registro compuesto
de una estructura llamada Lobo, que representa una jerarquía entre estos animales.
Los diferentes casos de lobos que forman la jerarquía son los siguientes:
Los cazadores poseen nombre, una lista de especies de presas cazadas y 3 lobos a cargo.
Los exploradores poseen nombre, una lista de nombres de territorio explorado (nombres de
bosques, ríos, etc.), y poseen 2 lobos a cargo.
Las crías poseen sólo un nombre y no poseen lobos a cargo.
La estructura es la siguiente:-}

type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre
data Manada = M Lobo

{-1. Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean
crías. Resolver las siguientes funciones utilizando recursión estructural sobre la estructura
que corresponda en cada caso:-}


manada1 = M (Cazador "caza1" ["presa1", "presa2"] 
                    (Explorador "Explo1" ["territorio1", "territorio2"] 
                                (Cria "cria1")
                                (Cria "cria2")
                                )
                    (Explorador "Explo2" ["territorio3", "territorio1"] 
                                (Cria "cria3")
                                (Cria "cria4")
                                )
                    (Cria "cria5")
                )

manada2 = M (Cazador "caza1" ["presa1", "presa2"] 
                (Explorador "Explo1" ["territorio1", "territorio2"] 
                            (Cria "cria1")
                            (Cria "cria2")
                            )
                (Explorador "Explo2" ["territorio3", "territorio1"] 
                            (Cria "cria3")
                            (Cazador "caza2" ["presa1", "presa2", "presa3"] 
                                    (Cria "cria4")
                                    (Cria "cria6")
                                    (Cria "cria7")
                                    )
                            )
                (Cria "cria5")
            )


--2. 
buenaCaza :: Manada -> Bool
buenaCaza   (M l)     =  cantDeCrias l < cantDePresas l

cantDeCrias :: Lobo -> Int
cantDeCrias (Explorador n ts l1 l2) = cantDeCrias l1 + cantDeCrias l2
cantDeCrias (Cazador n ps l1 l2 l3)    = cantDeCrias l1 + cantDeCrias l2 + cantDeCrias l3
cantDeCrias (Cria n )     = 1 

cantDePresas :: Lobo -> Int 
cantDePresas (Cria n )       = 0 
cantDePresas (Explorador n ts l1 l2) = cantDePresas l1 + cantDePresas l2
cantDePresas (Cazador n ps l1 l2 l3)=  contarPresas ps + cantDePresas l1 + cantDePresas l2 + cantDePresas l3

contarPresas :: [Presa] -> Int
contarPresas  []   = 0
contarPresas (p:ps) = 1 + contarPresas ps



--Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de crías.
--3. 

elAlfa :: Manada -> (Nombre, Int)
elAlfa   (M l)   =   elMayor (lobosYPresas l)


lobosYPresas :: Lobo -> [(Nombre, Int)]
lobosYPresas (Cria n )       = [(n, 0)] 
lobosYPresas (Explorador n ts l1 l2) = (n, 0) : lobosYPresas l1 ++ lobosYPresas l2
lobosYPresas (Cazador n ps l1 l2 l3) = (n, contarPresas ps) : lobosYPresas l1 ++ lobosYPresas l2 ++ lobosYPresas l3

elMayor :: [(Nombre, Int)] -> (Nombre, Int)
elMayor     (x:[])            = x
elMayor    (x:xs)         =  if (snd x) > (dameElSegundoDelHead xs)
                                then   elMayor (x : (eliminarHead xs))
                                else elMayor xs

eliminarHead :: [a] -> [a]
eliminarHead     [ ]   = []
eliminarHead    (x:xs) = xs

dameElSegundoDelHead ::  [(Nombre,Int)] -> Int
dameElSegundoDelHead    []    = 0
dameElSegundoDelHead   (x:xs) =  snd x


{-Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas
cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de
cero presas.-}
--4.
territorioIncluido :: Territorio -> [Territorio] ->Bool
territorioIncluido    t             []          = False
territorioIncluido    t            (te:tes)     = t == te || territorioIncluido t tes


exploradoresDeLaManada :: Territorio -> Lobo -> [Nombre]
exploradoresDeLaManada    t             (Cria n)     =[]
exploradoresDeLaManada    t  (Cazador n ps l1 l2 l3) =  (exploradoresDeLaManada t l1) ++ ( exploradoresDeLaManada t l2)
exploradoresDeLaManada    t  (Explorador n ts l1 l2) = if territorioIncluido t ts
                                                        then n : (exploradoresDeLaManada t l1) ++ (exploradoresDeLaManada t l2)
                                                        else (exploradoresDeLaManada t l1) ++ (exploradoresDeLaManada t l2)                


losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron    t             (M l)  = exploradoresDeLaManada t l

{-Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que
pasaron por dicho territorio.-}

--5. 
contieneNombre :: Nombre -> [Nombre] -> [Nombre]
contieneNombre    n         []        = [n]
contieneNombre    n         (no:nos) = if n == no
                                        then no : nos
                                        else no : contieneNombre n nos

nombresSinRepetir :: [Nombre] -> [Nombre] -> [Nombre]
nombresSinRepetir    []           ls  = ls
nombresSinRepetir    (n:ns)       ls  = contieneNombre n ls ++ nombresSinRepetir ns ls

agregarIndividual ::(Territorio, [Nombre]) -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregarIndividual       (t,ns)                   []                          = [(t,ns)]
agregarIndividual       (t,ns)                  ((t1,ns1): tups)             = if t==t1
                                                                                then ( t1, ( ns ++ ns1)) : tups
                                                                                else (agregarIndividual (t,ns) tups) 

agregarSinRepetir:: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregarSinRepetir       []                     tups = []
agregarSinRepetir   (tn:tns)                  tups                   = (agregarIndividual tn tups) ++  agregarSinRepetir tns tups

territorioExplorador:: [Territorio] -> Nombre ->[(Territorio, [Nombre])]
territorioExplorador    []             n          = []
territorioExplorador   (t:ts)          n      = (t,[n]) : territorioExplorador ts n

tuplasTerritorioExploradores :: Lobo -> [(Territorio, [Nombre])]
tuplasTerritorioExploradores (Cria n) = []
tuplasTerritorioExploradores (Explorador n ts l1 l2) = agregarSinRepetir(agregarSinRepetir (territorioExplorador ts n)  (tuplasTerritorioExploradores l1))(tuplasTerritorioExploradores l2)
tuplasTerritorioExploradores (Cazador n ps l1 l2 l3) = tuplasTerritorioExploradores l1++ tuplasTerritorioExploradores l2 ++ tuplasTerritorioExploradores l3 

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio     (M l)  =  tuplasTerritorioExploradores l

{-Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un territorio y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron
dicho territorio. Los territorios no deben repetirse.-}

--6. 
superioresDelCazador :: Nombre -> Manada -> [Nombre]
superioresDelCazador    n      (M l)   =   superioresDe n l

superioresDe:: Nombre -> Lobo -> [Nombre]
superioresDe   no        (Cria n )               = [] 
superioresDe   no        (Explorador n ts l1 l2) = (superioresDe no l1 )++(superioresDe no l2)
superioresDe   no        (Cazador n ps l1 l2 l3) = if n == no
                                                    then  []
                                                    else n: (superioresDe no l1 )++(superioresDe no l2)++(superioresDe no l3)      
     

{-Propósito: dado un nombre de cazador y una manada, indica el nombre de todos los
cazadores que tienen como subordinado al cazador dado (directa o indirectamente).
Precondición: hay un cazador con dicho nombre y es único.-}