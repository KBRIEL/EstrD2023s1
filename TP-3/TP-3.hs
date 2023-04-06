
--1 Tipos recursivos simples

data Color = Azul | Rojo
data Celda = Bolita Color Celda | CeldaVacia

bol1 = Bolita Rojo  CeldaVacia
bol2 = Bolita Azul  CeldaVacia
bol3 = Bolita Rojo (Bolita Azul (Bolita Azul(Bolita Rojo (Bolita Azul CeldaVacia))))

esColor :: Color -> Color-> Bool
esColor    Azul     Azul = True
esColor    Rojo     Rojo = True
esColor    _        _    = False

nroBolitas :: Color -> Celda -> Int
nroBolitas    c    CeldaVacia   = 0
nroBolitas    c    (Bolita c1 c2)  = if esColor c c1
                                        then 1 + nroBolitas c c2
                                        else nroBolitas c c2

{-
Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
existe una operación sobre listas que ayude a resolver el problema.-}

poner :: Color -> Celda -> Celda
poner    c     CeldaVacia   = Bolita c CeldaVacia
poner    c     (Bolita c1 c2) = Bolita c c2

{-Dado un color y una celda, agrega una bolita de dicho color a la celda.-}

sacar :: Color -> Celda -> Celda
sacar    _        CeldaVacia = CeldaVacia
sacar    Rojo     (Bolita Rojo c2 )= c2
sacar    Azul    ( Bolita Azul c2 )= c2
sacar    _        ce             = ce                                

{-Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
Gobstones, esta función es total.-}
ponerN :: Int -> Color -> Celda -> Celda
ponerN    0      c         ce         =  ce
ponerN    n      c         CeldaVacia = ponerN (n-1) c (Bolita c CeldaVacia)
ponerN    n      c         ce         = ponerN (n-1) c (Bolita c ce )

{-Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda

-}

--Camino hacia el tesoro----------------------------------------

data Objeto = Cacharro | Tesoro       deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino    deriving Show

cami1 = (Cofre [Tesoro] Fin)
cami2 = Fin
cami3 = Nada Fin
cami4 = (Cofre [Cacharro](Cofre [Tesoro](Nada Fin)))
cami5 = (Cofre [Cacharro, Tesoro](Cofre [Tesoro](Nada Fin)))
cami6 =  (Nada (Cofre [Tesoro] (Cofre [Tesoro] (Nada Fin))))


esTesoro :: Objeto -> Bool
esTesoro    Tesoro   = True
esTesoro    _        = False

contieneTesoro :: [Objeto] -> Bool
contieneTesoro    []       = False
contieneTesoro    (t:ts)   = esTesoro t || contieneTesoro ts

hayTesoro :: Camino -> Bool
hayTesoro    Fin          = False
hayTesoro    (Cofre os c)   = contieneTesoro os || hayTesoro c
hayTesoro    (Nada     c )  = hayTesoro c

{-Indica si hay un cofre con un tesoro en el camino.-}

{-
contarPasos :: Camino -> Int
contarPasos    Fin            = 0
contarPasos    (Cofre os c)   = if contieneTesoro os
                                    then 0
                                    else 1 + contarPasos c
contarPasos    (Nada c)       = 1 + contarPasos c
-}

pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro   Fin            = 0
pasosHastaTesoro   (Cofre os c)   = if contieneTesoro os then 0 else 1 + pasosHastaTesoro c
pasosHastaTesoro    (Nada c)      = 1 + pasosHastaTesoro c

{-
pasosHastaTesoro    ca = if hayTesoro ca 
                            then contarPasos ca  
                            else 0                                                                           
-}


{-Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
Precondición: tiene que haber al menos un tesoro.-}

moverN :: Int -> Camino -> Camino
moverN     0      c             =  c
moverN     n      Fin           =  error "no existen esa cantidad de pasos"
moverN     n     (Nada c)       =  moverN (n -1) c
moverN     n     (Cofre os c)   =  moverN (n -1) c

hayTesoroAqui :: Camino -> Bool
hayTesoroAqui    Fin         = False
hayTesoroAqui    (Cofre os c)= contieneTesoro os
hayTesoroAqui    (Nada c)    =  False

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn    0       ca    =  hayTesoroAqui   ca
hayTesoroEn    n       ca     = hayTesoroAqui (moverN n ca )


{-Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
pasos es 5, indica si hay un tesoro en 5 pasos.-}


cantTesoros :: Camino -> Int
cantTesoros    Fin      = 0
cantTesoros    (Cofre os c)= if contieneTesoro os 
                                then 1 + cantTesoros c 
                                else 0 + cantTesoros c 
cantTesoros    (Nada c)   = cantTesoros c


alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros    n      ca     = (cantTesoros ca) >= n




{-Indica si hay al menos “n” tesoros en el camino.-}

{-(desafío)-}
cortarCamino :: Camino -> Camino
cortarCamino    Fin   = error " no hay mas camino"
cortarCamino   ( Cofre os c) = c
cortarCamino   ( Nada c)    = c

agregarCamino ::Int -> Camino -> Camino
agregarCamino   n      Fin          = if (n>1) 
                                        then error "faltan caminos" 
                                        else Fin 
agregarCamino   n      (Cofre os c) = if (n>1) 
                                        then Cofre os (agregarCamino (n -1) c) 
                                        else Cofre os Fin
agregarCamino   n       (Nada c)     = if (n>1) 
                                        then Nada (agregarCamino (n -1) c) 
                                        else Nada Fin

caminoDesde :: Int -> Camino -> Camino
caminoDesde    0      c      = c
caminoDesde    n      c      = caminoDesde (n-1) (cortarCamino c)

caminoHasta :: Int -> Camino -> Camino
caminoHasta    0      c      = c
caminoHasta    n      c      = agregarCamino n c

caminoCorto :: Int -> Int -> Camino -> Camino
caminoCorto    n      m      ca     = caminoHasta m (caminoDesde n ca)

contarTesoros :: Camino -> Int
contarTesoros    Fin      = 0
contarTesoros    (Cofre ts c) = if contieneTesoro ts 
                                 then 1 + contarTesoros c 
                                 else contarTesoros c
contarTesoros    (Nada c)   = contarTesoros c

cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre     n    m       ca     = contarTesoros(caminoCorto n m ca)

{-Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
incluidos tanto 3 como 5 en el resultado.
-}

--Tipos arbóreos--------------------------------------------------------
arbol0 = EmptyT
arbol1 :: Tree Int
arbol1 = NodeT 2
                (NodeT 3 
                        EmptyT 
                        EmptyT
                        ) 
                (NodeT 1 
                    (NodeT 2 
                            EmptyT 
                            EmptyT
                            )  
                    EmptyT
                    )
arbol2 :: Tree Int
arbol2 = NodeT 1 
                (NodeT 3 
                        EmptyT 
                        EmptyT
                        ) 
                (NodeT 2 
                        EmptyT 
                        EmptyT
                        )

arbol3 :: Tree Int
arbol3 = NodeT 1
                (NodeT 2 
                        (NodeT 4 
                                EmptyT 
                                EmptyT) 
                                
                        EmptyT
                        )
                (NodeT 3 
                    (NodeT 5 
                            EmptyT 
                            EmptyT)  
                    EmptyT
                    )
--Árboles binarios-----------------------------------------------------
   
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show


--1. 
sumarT :: Tree Int -> Int
sumarT        EmptyT    = 0
sumarT        (NodeT n tl tr)     = n + sumarT (tl) + sumarT (tr)

{- Dado un árbol binario de enteros devuelve la suma entre sus elementos.-}

sizeT :: Tree a -> Int
sizeT        EmptyT    = 0
sizeT        (NodeT a tl tr)     = 1 + sizeT (tl) + sizeT (tr)


{-Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
en inglés).-}

mapDobleT :: Tree Int -> Tree Int
mapDobleT        EmptyT    = EmptyT
mapDobleT        (NodeT n tl tr)     = NodeT (n * 2) (mapDobleT tl) (mapDobleT tr)


{-Dado un árbol de enteros devuelve un árbol con el doble de cada número.-}

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT        n  EmptyT          = False
perteneceT        n  (NodeT n1 tl tr) = (n == n1)  || (perteneceT n tl) || (perteneceT n tr)


{-Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
árbol.-}

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT        a EmptyT    = 0
aparicionesT        a (NodeT b tl tr)     = if a == b 
                                                then 1 + aparicionesT a tl + aparicionesT a tr 
                                                else aparicionesT a tl + aparicionesT a tr 

{-Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
iguales a e.-}

esHoja :: Tree a -> Bool
esHoja     EmptyT = True
esHoja     _      = False

leaves :: Tree a -> [a]
leaves        EmptyT    = []
leaves        (NodeT a tl tr)     = if esHoja tl && esHoja tr
                                     then [a] 
                                     else leaves tl ++ leaves tr


{-Dado un árbol devuelve los elementos que se encuentran en sus hojas.-}

heightT :: Tree a -> Int
heightT     EmptyT  = 0
heightT     (NodeT a tl tr)     = 1 + heightT tl + heightT tr 

{-Dado un árbol devuelve su altura.
Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad
de niveles del árbol1
. La altura para EmptyT es 0, y para una hoja es 1.-}

mirrorT :: Tree a -> Tree a
mirrorT     EmptyT    = EmptyT
mirrorT     (NodeT a tl tr)     = NodeT a  (mirrorT tr) (mirrorT tl)

{-Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho,
en cada nodo del árbol.-}

toList :: Tree a -> [a]
toList    EmptyT = []
toList    (NodeT a tl tr)     =  toList tl ++ [a] ++ toList tr

{-Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz
y luego los elementos del hijo derecho.-}

levelN :: Int -> Tree a -> [a]
levelN     _      EmptyT          =  [] 
levelN     0     (NodeT x _  _ )  =  x : []
levelN     n     (NodeT _ t1 t2)  =  levelN (n-1) t1 ++ levelN (n-1) t2

{-Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un
nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la
distancia de la raiz a uno de sus hijos es 1.
Nota: El primer nivel de un árbol (su raíz) es 0.-}

nivel :: [[a]] -> [[a]] -> [[a]]
nivel []       ys      = ys
nivel xs      []       = xs
nivel (x:xs) (y:ys) = (x ++ y) : nivel xs ys

listPerLevel :: Tree a -> [[a]]
listPerLevel    EmptyT  = []
listPerLevel    (NodeT x t1 t2) = [x] : nivel (listPerLevel t1) (listPerLevel t2)

{-Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de
dicho árbol.-}

ramaMasLarga :: Tree a -> [a]
ramaMasLarga    EmptyT  = []
ramaMasLarga    (NodeT x tl tr) = if heightT tl > heightT tr
                                     then  x: ramaMasLarga tl 
                                     else  x: ramaMasLarga tr

{-Devuelve los elementos de la rama más larga del árbol-}

consACada :: a -> [[a]] -> [[a]]
consACada x []       = []
consACada x (xs:xss) = (x:xs) : consACada x xss

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos    EmptyT  = []
todosLosCaminos    (NodeT x t1 t2) =[x] : consACada x (todosLosCaminos t1) ++ consACada x (todosLosCaminos t2)


{-Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raiz hasta las hojas.
-}

--Expresiones Aritméticas-------------------------------------------------------------------------
data ExpA = Valor Int
                    | Sum ExpA ExpA
                    | Prod ExpA ExpA
                    | Neg ExpA


eval :: ExpA -> Int
eval    (Valor n )  =  n
eval    (Sum  (Valor n)(Valor m)) =  n + m 
eval    (Prod (Valor n)(Valor m)) =  n * m
eval    (Neg  (Valor n))          = -n 


{-Dada una expresión aritmética devuelve el resultado evaluarla.-}

simplificar ::  ExpA -> ExpA
simplificar    (Valor n)                    = (Valor n)
simplificar    (Sum (Valor n1) (Valor 0) )  = (Valor n1)
simplificar    (Sum (Valor 0) (Valor n2) )  = (Valor n2)
simplificar    (Sum (Valor n1) (Valor n2) ) = (Valor (n1 + n2))
simplificar    (Prod (Valor 0) (Valor n2) ) = (Valor 0)
simplificar    (Prod (Valor n1) (Valor 0) ) = (Valor 0)
simplificar    (Prod (Valor n1) (Valor n2)) = (Valor (n1 * n2))
simplificar    (Neg (Neg (Valor n1)))      = (Valor n1 )

{-Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando
notación matemática convencional):
a) 0 + x = x + 0 = x
b) 0 * x = x * 0 = 0
c) 1 * x = x * 1 = x
d) - (- x) = x
-}