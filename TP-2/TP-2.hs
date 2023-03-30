--1 Recursión sobre listas
--1)
sumatoria :: [Int] -> Int
sumatoria     []    = 0
sumatoria    (x:xs) = x + sumatoria xs

--2)
longitud ::  [a]  -> Int
longitud    []    =  0
longitud    (x:xs)= 1 + longitud xs

--3) 
sucesores :: [Int]  -> [Int]
sucesores    []     =  []
sucesores    (x:xs) = (x + 1): sucesores xs 

--4)
conjuncion :: [Bool] -> Bool
-- la lista no debe de ser vacia
conjuncion    []     = True
conjuncion    (x:xs) = x && conjuncion xs

--5)
disyuncion :: [Bool] -> Bool
-- la lista no debe de ser vacia
disyuncion    []     = False
disyuncion    (x:xs) = x || disyuncion xs

--6)
aplanar :: [[a]]  -> [a]
aplanar    []     = []
aplanar    (x:xs) = x ++ aplanar xs

--7)
pertenece :: Eq a => a -> [a] -> Bool
pertenece     a    []         = False
pertenece     a    (x:xs)     =   a == x || pertenece a xs

--8)
apariciones :: Eq a => a -> [a] -> Int
apariciones     a    []         =  0
apariciones     a    (x:xs)     = if a == x
                                    then 1 + apariciones a xs
                                    else apariciones a xs

--9)
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA     n     []    = []
losMenoresA     n     (x:xs)= if n > x
                                then x : losMenoresA n xs
                                else losMenoresA n xs

--10)
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA    n      []    = []
lasDeLongitudMayorA    n      (x:xs)= if n < length x
                                         then x : lasDeLongitudMayorA n xs    
                                         else lasDeLongitudMayorA n xs
--11)
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal    [ ]    a = a: []
agregarAlFinal    (x:xs) a = x: agregarAlFinal xs a 


--12)
agregar :: [a] -> [a]    -> [a]
agregar    []      []    = []
agregar    ls      []    = ls
agregar    []      xs    = xs
agregar    ls    (x:xs)  =  agregar (agregarAlFinal ls x) xs

--13)
reversa :: [a]    -> [a]
reversa    []     = []
reversa    (x:xs) = agregarAlFinal (reversa xs) x

--14)
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos    []    []    = []
zipMaximos    xs    []    = xs
zipMaximos    []    ys    = ys
zipMaximos    (x:xs)(y:ys)=  if x > y
                                then x: zipMaximos xs ys 
                                else y: zipMaximos xs ys

--15)

elMinimo :: Ord a => [a] -> a
--no debe haber un lista vacia
elMinimo    []    = error "El mínimo de una lista vacía es error"
elMinimo  (x:[])  = x 
elMinimo  (x:xs)  = if x < elMinimo xs
                       then x   
                       else elMinimo xs 
    

--2. Recursión sobre números

--1)
factorial :: Int -> Int
factorial    0   = 1
factorial    n   = factorial (n - 1) * n

--2)
cuentaRegresiva :: Int -> [Int]
--n tiene que ser mayor a 0
cuentaRegresiva    0    = []
cuentaRegresiva    n    = n: cuentaRegresiva (n - 1)


--3)
repetir :: Int -> a -> [a]
repetir    0    a    = []
repetir    n    a    = a: repetir (n - 1) a

--4)
losPrimeros :: Int -> [a] -> [a]
losPrimeros    n    []    =[]
losPrimeros    0    xs    =[]
losPrimeros    n  (x:xs)  = x: losPrimeros (n-1) xs

--5)
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros    n    []    =  []
sinLosPrimeros    0    xs    =  xs
sinLosPrimeros    n  (x:xs)  =  sinLosPrimeros (n-1) xs 


--3  Registros

--1)
data Persona = P String Int deriving Show
--persona--
cris :: Persona  
cris = P "Cris" 23
agus :: Persona  
agus = P "agus" 44
mora :: Persona  
mora = P "mora" 10


--funciones complementarias--------------------------------------------
edad :: Persona -> Int
edad     (P n e) = e

esMismoTipoPK :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipoPK     Agua           Agua           = True
esMismoTipoPK     Fuego          Fuego          = True
esMismoTipoPK     Planta         Planta         = True
esMismoTipoPK     _              _              = False

tipoDePokemonEsSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
tipoDePokemonEsSuperior Agua  Fuego  = True
tipoDePokemonEsSuperior Fuego Planta = True
tipoDePokemonEsSuperior Planta Agua  = True
tipoDePokemonEsSuperior _       _    = False

------------------------------------------------------------------------------------------------------

--a
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA     n      []       = []
mayoresA     0      xs       =  xs
mayoresA     n      (x:xs)   = if n < edad x
                                  then x: mayoresA n xs
                                  else mayoresA n xs

--b                                  
lsEdades :: [Persona] -> [Int]
lsEdades     []       =[]
lsEdades     (x:xs)   = (edad x): lsEdades xs

promedio  :: [Int] -> Int
promedio     []    = 0
promedio     xs    = div (sumatoria xs)  (length xs)

promedioEdad :: [Persona] -> Int
promedioEdad    []        = 0 
promedioEdad    xs    = promedio (lsEdades xs)

--c
elMasViejo :: [Persona] -> Persona
-- la lista debe conteter al menos una perona
elMasViejo    [ ]       = error " no existe persona en la lista"
elMasViejo    ( x: [])  = x
elMasViejo    (x:xs)    = if edad x > edad (elMasViejo xs)
                            then x
                            else elMasViejo xs


--2)
data TipoDePokemon = Agua | Fuego | Planta    deriving  Show
data Pokemon = ConsPokemon TipoDePokemon Int   deriving  Show
data Entrenador = ConsEntrenador String [Pokemon]  deriving  Show

-- pokemons---
pok1 :: Pokemon 
pok1 = ConsPokemon Agua 100 
pok2 :: Pokemon 
pok2 = ConsPokemon Fuego 80
pok3 :: Pokemon 
pok3 = ConsPokemon Planta 90 
pok4 :: Pokemon 
pok4 = ConsPokemon Agua 70
--entrenadores--------
entrenador1 :: Entrenador
entrenador1 = ConsEntrenador "Nico" [pok1, pok2]
entrenador2 :: Entrenador
entrenador2 = ConsEntrenador "Gus" [pok2, pok3]
entrenador3 :: Entrenador
entrenador3 = ConsEntrenador "Isra" [pok1,pok2, pok3, pok4]

--a
cantPokemon :: Entrenador -> Int
cantPokemon    (ConsEntrenador n [])   = 0
cantPokemon    (ConsEntrenador n ps)   = longitud ps

--b
mismoTipo :: TipoDePokemon -> Pokemon -> Bool
mismoTipo    tp              (ConsPokemon  t n ) = esMismoTipoPK tp t

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe    tp             (ConsEntrenador n [])     = 0
cantPokemonDe    tp             (ConsEntrenador n (p:ps)) = if mismoTipo tp p
                                                                 then  1 + cantPokemonDe tp (ConsEntrenador n ps)
                                                                 else cantPokemonDe tp (ConsEntrenador n ps)        
--c

hayEnListaPokTipo :: TipoDePokemon -> [Pokemon] ->    Bool
hayEnListaPokTipo     tp               []      = False
hayEnListaPokTipo     tp               (p:ps)  = if ( mismoTipo tp p)
                                                                then  True
                                                                else False || hayEnListaPokTipo tp ps
                                                            
hayPokTipo :: TipoDePokemon -> Entrenador           ->    Bool
hayPokTipo     tp              (ConsEntrenador n [])= False
hayPokTipo     tp              (ConsEntrenador n ps)= hayEnListaPokTipo tp ps

superaA :: TipoDePokemon -> Pokemon               -> Bool
superaA     tp             (ConsPokemon tp1 ent1) = tipoDePokemonEsSuperior tp tp1

tipoVenceLista :: TipoDePokemon -> [Pokemon] -> Bool
tipoVenceLista   tp                []        = False
tipoVenceLista   tp                ( p:ps)   = if (superaA tp p)
                                                then  True && tipoVenceLista tp ps
                                                else False                          

tipoVenceTodos :: TipoDePokemon -> Entrenador           -> Bool
tipoVenceTodos        tp               (ConsEntrenador n ps) = tipoVenceLista tp ps


cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador          ->  Entrenador            -> Int
cuantosDeTipo_De_LeGananATodosLosDe_    tp               (ConsEntrenador n1 [])  e2                 =  0 
cuantosDeTipo_De_LeGananATodosLosDe_    tp               e1                     (ConsEntrenador n2 []) =  0
cuantosDeTipo_De_LeGananATodosLosDe_    tp               e1                      e2                    =  if( (hayPokTipo tp e1)&& (tipoVenceTodos tp e2))
                                                                                                            then  cantPokemonDe tp e1
                                                                                                            else 0
--d
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon    (ConsEntrenador n ps) = hayEnListaPokTipo Agua ps 
                                            && hayEnListaPokTipo Fuego ps 
                                            && hayEnListaPokTipo Planta ps

--3)
data Seniority = Junior | SemiSenior | Senior  deriving  Show
data Proyecto = ConsProyecto String  deriving  Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto  deriving  Show
data Empresa = ConsEmpresa [Rol]  deriving  Show


proyecto1 = ConsProyecto "proyecto1"
proyecto2 = ConsProyecto "proyecto2"
proyecto3 = ConsProyecto "proyecto3"
rol1 = Developer Junior proyecto1
rol2 = Developer Junior proyecto2
rol3 = Developer Senior proyecto3
rol4 = Developer Senior proyecto3
empresa1 = ConsEmpresa [rol1, rol2]
empresa2 = ConsEmpresa [rol1, rol3]
empresa3 = ConsEmpresa [rol1, rol2, rol3]
empresa4 = ConsEmpresa [rol1, rol2, rol3, rol4]

--a

dameElNombre :: Proyecto -> String
dameElNombre   (ConsProyecto s)= s

proyectosSinRepetir ::[Proyecto]-> [Proyecto]
proyectosSinRepetir [] = []
proyectosSinRepetir   (x:xs) = if contiene x xs
                            then proyectosSinRepetir xs
                            else x: proyectosSinRepetir xs 

contiene :: Proyecto -> [Proyecto] -> Bool
contiene    a     [] = False
contiene    a    (x:xs) = if dameElNombre a == dameElNombre x
                            then True || contiene a xs
                            else  contiene a xs

rolAProyecto :: Rol -> Proyecto
rolAProyecto     (Developer s1 p1)  = p1
rolAProyecto     (Management s2 p2) = p2

proyectols :: [Rol] -> [Proyecto]
proyectols     []   = []
proyectols    (r:rs)= (rolAProyecto r): ( proyectols rs)

proyectos :: Empresa -> [Proyecto]
proyectos    (ConsEmpresa []) = []
proyectos    (ConsEmpresa rs) =  proyectosSinRepetir (proyectols rs)

{-Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos-}


--b
esDeveloper :: Rol -> Bool  
esDeveloper (Developer _ _ )  = True
esDeveloper  _               = False

esSenior :: Rol         -> Bool 
esSenior ( Developer  Senior _)  = True
esSenior  _              = False

sonSenior :: [Rol] -> [Rol]
sonSenior       [] = []
sonSenior   (r:rs) = if esDeveloper r && esSenior r
                        then r: sonSenior rs
                        else  sonSenior rs

dameElProyecto :: Rol -> Proyecto
--debe ser developer senior
dameElProyecto   (Developer s p)  =  p
                           
nombreDelProyecto :: Proyecto         ->  String
nombreDelProyecto    (ConsProyecto s) = s

estaEnElProyecto :: Rol -> [Proyecto] -> Bool
estaEnElProyecto    rol     []      =    False
estaEnElProyecto    rol    (p:ps)   =   nombreDelProyecto (dameElProyecto rol) == nombreDelProyecto p || estaEnElProyecto  rol ps

soloLosDeLosProyectos :: [Rol] -> [Proyecto] -> [Rol]
soloLosDeLosProyectos    []       []          = []
soloLosDeLosProyectos    (r:rs)   ps          = if estaEnElProyecto r ps
                                                  then r: soloLosDeLosProyectos rs  ps      
                                                  else soloLosDeLosProyectos rs ps


losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa []) ps = 0
losDevSenior (ConsEmpresa rs) ps = length (sonSenior rs)



{-Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen
además a los proyectos dados por parámetro.
-}


--c

dameProyRol :: Rol -> String
dameProyRol   (Developer s p)  = dameElNombre p
dameProyRol   (Management s p) = dameElNombre p

perteneceA :: [Proyecto]-> Rol-> Bool
perteneceA    []            r  = False
perteneceA     (p:ps)       r  =  if dameProyRol r == dameElNombre p
                                    then True
                                    else False
                                    

pertenecenAEstosProyectos :: [Proyecto]-> [Rol]->[Rol]
pertenecenAEstosProyectos    []            []    = []
pertenecenAEstosProyectos    []            rs    = []
pertenecenAEstosProyectos    ps            []    = []
pertenecenAEstosProyectos     ps           (r:rs)=  if perteneceA ps r
                                                        then r: pertenecenAEstosProyectos ps rs
                                                        else pertenecenAEstosProyectos ps rs

trabajadoresDelProyecto  :: [Proyecto] -> Empresa ->[Rol]
trabajadoresDelProyecto      []            e      = []
trabajadoresDelProyecto      ps        (ConsEmpresa rs)      =  pertenecenAEstosProyectos ps rs  

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn    []            e       =  0
cantQueTrabajanEn    ps       e       = length (trabajadoresDelProyecto ps e)

{-Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.-}


--d

todosLosProyectos :: [Rol]  -> [Proyecto]
todosLosProyectos    []     =  []
todosLosProyectos    (r:rs) =  ( dameElProyecto r) : todosLosProyectos rs

emleadosDelProyecto :: Rol -> [(Proyecto, Rol)]
emleadosDelProyecto (Developer s p)  = (p,(Developer s p)): [ ]
emleadosDelProyecto (Management s p) = (p,(Management s p)): [ ]

separarTupla :: (Proyecto, Rol) -> Proyecto
--debe ser una tupla Proyecto Rol
separarTupla     (p,r)           = p

cantEmpleadosXProyecto :: Proyecto -> [(Proyecto, Rol)] -> Int
cantEmpleadosXProyecto     p             []             = 0
cantEmpleadosXProyecto     p           (r:rs)           = if dameElNombre p == dameElNombre (separarTupla r) 
                                                            then 1 + cantEmpleadosXProyecto p rs 
                                                            else  cantEmpleadosXProyecto p rs 

cantEmleadosDelProyecto :: [(Proyecto, Rol)] ->[Proyecto]  -> [ (Proyecto, Int)]
cantEmleadosDelProyecto   prs        (p:ps)  =  (p , (cantEmpleadosXProyecto p prs)) : (cantEmleadosDelProyecto prs ps) 

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto    (ConsEmpresa (r:rs)) = cantEmleadosDelProyecto (emleadosDelProyecto r) (proyectosSinRepetir(todosLosProyectos (r:rs)))


{-Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
cantidad de personas involucradas.-}



