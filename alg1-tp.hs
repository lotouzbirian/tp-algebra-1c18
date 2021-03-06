-----------------------------------------------------------------------
-- Nombre y LU/DNI de los integrantes del grupo:
-- INTEGRANTE 1: Avellaneda, Matías Alejandro (L.U.: 138/18)
-- INTEGRANTE 2: Larregui, Nicolás Alejandro (L.U.: 133/18)
-- INTEGRANTE 3: Otouzbirian, Leandro Ariel (L.U.: 5/18)
-----------------------------------------------------------------------

data Desplazamiento = Arriba | Abajo | Izquierda | Derecha deriving (Show, Eq)

type Conjunto a = [a]
type Camino = [Desplazamiento]
type Posicion = (Integer,Integer)
type Tablero a = [[a]]
type CampoMinado = Tablero Bool
type TableroAF = Tablero Desplazamiento

-- Devuelve el tamaño de un tablero.
tamano :: Tablero a -> Integer
tamano t = fromIntegral(length t)

-- Devuelve el valor de una posición de un tablero.
-- Notar que la primera posición de arriba a la izquierda es la (1,1).
valor :: Tablero a -> Posicion -> a
valor t (i,j) = iesimo (iesimo t i) j

-- Devuelve el iésimo elemento de una lista. El primer elemento es el 1.
iesimo :: [a] -> Integer -> a
iesimo (x:xs) 1 = x
iesimo (x:xs) n = iesimo xs (n-1)

-- Determina si una posición está dentro de los límites de un tablero.
posValida :: Tablero a -> Posicion -> Bool
posValida t (i,j) = 1<=i && i<=n && 1<=j && j<=n
    where n = tamano t
    
    
-- Funciones de ejemplo, solo para ilustrar cómo usar los tipos definidos arriba.
-- Determina si un desplazamiento es vertical (Arriba o Abajo).
esVertical :: Desplazamiento -> Bool
esVertical Arriba = True
esVertical Abajo = True
esVertical _ = False

-- Cuenta la cantidad de Desplazamientos verticales en un Camino.
contarDesplazamientosVerticales :: Camino -> Integer
contarDesplazamientosVerticales [] = 0
contarDesplazamientosVerticales (x:xs) | esVertical x = 1 + resto
                                       | otherwise    = resto
  where resto = contarDesplazamientosVerticales xs

-- Caminos de prueba.
camino1 = [Derecha, Abajo, Izquierda, Arriba, Abajo, Abajo, Derecha, Derecha]
camino2 = [Derecha, Abajo, Derecha, Abajo]
camino3 = [Derecha, Abajo, Derecha, Izquierda, Derecha, Abajo]

-- CampoMinado de prueba.
campo1 :: CampoMinado
campo1 = [ [False, False, True],
           [True,  False, False],
           [True,  True,  False] ]

-- TableroAF de prueba, sin ciclos.
taf1 :: TableroAF
taf1 = [ [Derecha,  Derecha, Abajo],
         [Arriba, Izquierda, Abajo],
         [Arriba, Izquierda, Abajo] ]

-- TableroAF de prueba, con ciclos.
taf2 :: TableroAF
taf2 = [ [Derecha,       Abajo, Abajo],
         [Arriba,    Izquierda, Abajo],
         [Izquierda, Izquierda, Izquierda] ]
        
         
         
         
         
         
-- Parte A. Campos Minados

-- Devuelve la nueva posicion de un RAE luego de realizar un desplazamiento.
-- Esta función también se utiliza en la parte b.
posicionNueva :: Posicion -> Desplazamiento -> Posicion
posicionNueva (x,y) Arriba = (x - 1,y)
posicionNueva (x,y) Abajo = (x + 1,y)
posicionNueva (x,y) Izquierda = (x,y - 1)
posicionNueva (x,y) Derecha = (x,y + 1)

-- Devuelve la posicion de un RAE después de haber realizado el n-ésimo desplazamiento de un camino.
posicion :: Camino -> Integer -> Posicion
posicion _ 0 = (1,1)
posicion (c:cs) n = posicionNueva (posicion (c:cs) (n-1)) (iesimo (c:cs) n)

-- Crea una lista con las sucesivas posiciones que toma un RAE al recorrer un camino a partir de cierto desplazamiento.
posicionesDesde :: Camino -> Integer -> [Posicion]
posicionesDesde cs n | n == fromIntegral(length cs) = [posicion cs n]
                     | otherwise = posicion cs n : posicionesDesde cs (n+1)

-- Crea una lista con las sucesivas posiciones que toma un RAE al recorrer un camino desde la posicion inicial.
posiciones :: Camino -> [Posicion]
posiciones cs = posicionesDesde cs 0

-- Determina si cada posición que toma un RAE al recorrer un camino lo mantiene dentro de los límites del tablero.
sonTodasValidas :: CampoMinado -> [Posicion] -> Bool
sonTodasValidas cm c | length c == 1 && posValida cm (head c) = True
                     | posValida cm (head c) = sonTodasValidas cm (tail c)
                     | otherwise = False

-- Dados un campo minado y una posición (n,m), crea una lista con todas las posiciones que contienen minas empezando por (n,m).
-- Recorre cada fila de izquierda a derecha y cuando termina una pasa a la siguiente.
posicionesConMinas :: CampoMinado -> Posicion -> [Posicion]
posicionesConMinas cm (n,m) | n > tamano cm = []
                            | m > tamano cm = posicionesConMinas cm (n + 1,1)
                            | valor cm (n,m) = (n,m) : posicionesConMinas cm (n,m + 1)
                            | otherwise = posicionesConMinas cm (n,m + 1)
                          
-- Decide si dos listas de posiciones tienen algún elemento en común. Devuelve False en caso afirmativo.
sonDisjuntas :: [Posicion] -> [Posicion] -> Bool
sonDisjuntas c1 c2 | length c1 == 0 = True
                   | elem (head c1) c2 = False
                   | otherwise = sonDisjuntas (tail c1) c2

-- Determina si un RAE recorre un camino sin pisar ninguna mina. Devuelve True en caso afirmativo.
noExplota :: CampoMinado -> Camino -> Bool
noExplota cm c = sonDisjuntas (posicionesConMinas cm (1,1)) (posiciones c)

-- Devuelve la posición en la que se encuentra un RAE al terminar de recorrer un camino.
posicionFinal :: [Posicion] -> Posicion
posicionFinal c | length c == 1 = head c
                | otherwise = posicionFinal (tail c)

-- Determina si una lista de posiciones tiene elementos repetidos.
-- Esta función también se utiliza en la parte b.
hayRepetidos :: [Posicion] -> Bool
hayRepetidos [] = False
hayRepetidos (p:ps) | elem p ps = True
                    | otherwise = hayRepetidos ps

-- Toma un elemento n de tipo a y una lista de listas de tipo a, y agrega n a cada una de las listas.
agregarATodas :: a -> [[a]] -> [[a]]
agregarATodas _ [] = []
agregarATodas n (x:xs) = (n:x) : (agregarATodas n xs)

-- Toma una lista de elementos de tipo a y una listas de listas de tipo a, y agrega cada elemento de la primera a cada lista de la segunda.
agregarTodosATodas :: [a] -> [[a]] -> [[a]] 
agregarTodosATodas [] _ = []
agregarTodosATodas (x:xs) yys = (agregarATodas x yys) ++ (agregarTodosATodas xs yys)

-- Dado un conjunto y un entero k, devuelve todas las combinaciones de longitud k que pueden formarse con los elementos del conjunto.
variaciones :: Conjunto a -> Integer -> Conjunto [a]
variaciones _ 0 = [[]]
variaciones xs k = agregarTodosATodas xs (variaciones xs (k - 1))

-- Dado un conjunto de caminos y un campo minado, devuelve todos los caminos validos del conjunto cuya posición final es (n,n).
caminosDeLongitudK :: Conjunto Camino -> CampoMinado -> Conjunto Camino
caminosDeLongitudK [] _ = []
caminosDeLongitudK (x:xs) cm | caminoDeSalida cm x = x : caminosDeLongitudK xs cm
                             | otherwise = caminosDeLongitudK xs cm


-- Determina si un camino se mantiene dentro de los límites del tablero a lo largo de su trayectoria,
-- asumiendo que se comenzará por la posición (1,1).
caminoValido :: CampoMinado -> Camino -> Bool
caminoValido cm c = sonTodasValidas cm (posiciones c)

-- Determina si un RAE, comenzando en la posición (1,1), al seguir el camino dado,
-- llega a la posición (n,n) sin pisar ninguna mina.
caminoDeSalida :: CampoMinado -> Camino -> Bool
caminoDeSalida cm c = caminoValido cm c && noExplota cm c && posicionFinal (posiciones c) == (tamano cm, tamano cm)

-- Determina si un RAE, comenzando en la posición (1,1), al seguir el camino dado,
-- llega a la posición (n,n) sin pisar ninguna mina y sin pasar dos veces por una misma posición.
caminoDeSalidaSinRepetidos :: CampoMinado -> Camino -> Bool
caminoDeSalidaSinRepetidos cm c = caminoDeSalida cm c && not(hayRepetidos (posiciones c))

-- Dados un campo minado y un número natural k, devuelve el conjunto de todos los caminos de
-- longitud k que lleven a un RAE desde (1,1) hasta (n,n), sin pisar ninguna mina.
salidasEnKDesp :: CampoMinado -> Integer -> Conjunto Camino
salidasEnKDesp cm k = caminosDeLongitudK (variaciones [Arriba, Abajo, Derecha, Izquierda] k) cm






-- Parte B. Siga la flecha.
-- Tableros estáticos

-- Dados un tablero y una posición p, devuelve una lista que contiene las posiciones por las que pasará
-- un AF si se lo coloca inicialmente sobre p. 
recorrido :: TableroAF -> Posicion -> [Posicion]
recorrido t (x,y) | posValida t (posicionNueva (x,y) (valor t (x,y))) = (x,y) : recorrido t (posicionNueva (x,y) (valor t (x,y)))
                  | otherwise = [(x,y)]
                  
-- Dados un tablero y una posición p, determina si al colocar un AF en p, el AF escapará del tablero o entrará en un loop infinito.
escapaDelTablero :: TableroAF -> Posicion -> Bool
escapaDelTablero t (x,y) = not(hayRepetidos (take (fromInteger(tamano t) ^ 2 + 1) (recorrido t (x,y))))

-- Tableros dinámicos

-- Dados una lista, un entero n y un elemento del tipo de la lista x, reemplaza el n-ésimo elemento de la lista por x.
reemplazar :: [a] -> Integer -> a -> [a]
reemplazar xs n x | n == 1 = x : tail xs
                  | otherwise = head xs : reemplazar (tail xs) (n - 1) x

-- Dados un tablero, una posición y un desplazamiento, cambia el desplazamiento correspondiente a esa posición por el nuevo.
cambiarPosicion :: TableroAF -> Posicion -> Desplazamiento -> TableroAF
cambiarPosicion t (x,y) d = reemplazar t x (reemplazar (iesimo t x) y d)

-- Dados un tablero y una posición, devuelve un nuevo tablero con la posición nueva según el sentido horario.
tableroNuevo :: TableroAF -> Posicion -> TableroAF
tableroNuevo t (x,y) | valor t (x,y) == Arriba = cambiarPosicion t (x,y) Derecha
                     | valor t (x,y) == Derecha = cambiarPosicion t (x,y) Abajo
                     | valor t (x,y) == Abajo = cambiarPosicion t (x,y) Izquierda
                     | valor t (x,y) == Izquierda = cambiarPosicion t (x,y) Arriba

-- Dados un tablero y una posición p, devuelve cuantas veces tiene que desplazarse un AF para escapar
-- del tablero si inicialmente lo colocamos en p.
cantidadDePasosParaSalir :: TableroAF -> Posicion -> Integer
cantidadDePasosParaSalir t (x,y) | not(posValida t (posicionNueva (x,y) (valor t (x,y)))) = 1
                                 | otherwise = 1 + cantidadDePasosParaSalir (tableroNuevo t (x,y)) (posicionNueva (x,y) (valor t (x,y)))
