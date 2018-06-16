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

-- Crea una lista con todas las posiciones del campo minado que tienen minas.
posicionesConMinas :: CampoMinado -> Posicion -> [Posicion]
posicionesConMinas cm (n,m) | n > tamano cm = []
                            | valor cm (n,m) && m <= tamano cm = (n,m) : posicionesConMinas cm (n,m + 1)
                            | m <= tamano cm = posicionesConMinas cm (n,m + 1)
                            | otherwise = posicionesConMinas cm (n + 1,1)
                          
-- Decide si dos listas de posiciones tienen algún elemento en común. Devuelve False en caso afirmativo.
sonDisjuntas :: [Posicion] -> [Posicion] -> Bool
sonDisjuntas c1 c2 | length c1 == 0 = True
                   | elem (head c1) c2 = False
                   | otherwise = sonDisjuntas (tail c1) c2

-- Determina si un RAE recorre un camino sin pisar ninguna mina. Devuelve True en caso afirmativo.
noExplota :: CampoMinado -> Camino -> Bool
noExplota cm c = sonDisjuntas (posicionesConMinas cm) (posiciones c)

-- Devuelve la posición en la que se encuentra un RAE al terminar de recorrer un camino.
posicionFinal :: [Posicion] -> Posicion
posicionFinal [] = (1,1)
posicionFinal c | length c == 1 = head c
                | otherwise = posicionFinal (tail c)

-- Determina si una lista de posiciones tiene elementos repetidos.
hayRepetidos :: [Posicion] -> Bool
hayRepetidos [] = False
hayRepetidos (p:ps) | elem p ps = True
                    | otherwise = hayRepetidos ps

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
