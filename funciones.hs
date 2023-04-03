module Funciones (Casilla(V,VN,Temp), Tablero(T), end_game, insertar, insertar_valor, no_existe_solucion,
	eliminar_valor, valido, anterior_index, siguiente_index, get, montar, desmontar, es_fijo, longitud,
	leer_tablero, print_tablero, sacar_valores, actualizar_posibilidades, mapCasillasVacias) where



data Casilla = V Int | VN Int | Temp [Int]
	deriving (Show, Read, Eq)

data Tablero = T [[Casilla]] 
	deriving (Show, Read, Eq)

 

------------------------ COMPROBACIONES DE FIN DE JUEGO ------------------------

-- comprueba si el tablero está lleno
end_game :: Tablero -> Bool
end_game (T tab) = null (concat $ map (filter esta_vacio) tab)

-- comprueba si una casilla está vacía
esta_vacio :: Casilla -> Bool
esta_vacio (V x) = x == 0
esta_vacio (Temp xs) = True
esta_vacio (VN _) = False

-- comprueba si el tablero tiene solución (no la tiene si alguna casilla tiene 0 posibilidades)
no_existe_solucion :: Tablero -> Bool
no_existe_solucion (T tab) = or (concat $ map (map (null.sacar_valores)) tab)

----------------------- INTRODUCIR Y ELIMINAR DEL TABLERO -----------------------

insertar :: Int -> (Int,Int) -> Tablero -> Tablero
insertar x (i,j) (T tab) = insertar_valor (V x) (i,j) (T tab)
		

-- NO es para elimanar una casilla (para eso usaremos "insertar 0 ..."), si no para eliminar 
-- posibilidades de un "Temp"
eliminar_valor :: Int -> (Int,Int) -> Tablero -> Tablero
eliminar_valor x (i,j) (T tab)
	| longitud casilla == 0 = T tab
	| otherwise = insertar_valor nuevo_valor (i,j) (T tab)
	where 
		casilla = get (i,j) (T tab)
		posibles = sacar_valores casilla
		nuevo_valor = Temp (filter (/= x) posibles)


-- meter un valor nuevo en el tablero
insertar_valor :: Casilla -> (Int,Int) -> Tablero -> Tablero
insertar_valor nuevo_valor (i,j) (T tab) = T ((take (i-1) tab) ++ [nueva_fila] ++ (drop i tab))
	where 
		nueva_fila = (take (j-1) fila) ++ [nuevo_valor] ++ (drop j fila)
		fila = tab !! (i-1)
		

------------------------- VER SI EL VALOR A INTRODUCIR ES VÁLIDO -------------------------

valido :: Int -> (Int,Int) -> Tablero -> Bool
valido x (i,j) tab
	| es_fijo (i,j) tab = False
	| otherwise = and [(_comprobar_fila x tab i),(_comprobar_columna x tab j),(_comprobar_bloque x tab (i,j))]


-- Comprobamos la fila, columna y el bloque, cada una se crea una lista de todas las casillas involucradas 
-- y luego comprueban con "_comprobar" que "x" no este en dicha lista.

_comprobar_fila :: Int -> Tablero -> Int -> Bool
_comprobar_fila x (T tab) i = _comprobar x (last $ take i tab)

_comprobar_columna :: Int -> Tablero -> Int -> Bool
_comprobar_columna x (T tab) j = _comprobar x (map (last.(take j)) tab)

_comprobar_bloque :: Int -> Tablero -> (Int,Int) -> Bool
_comprobar_bloque x (T tab) (i,j) = _comprobar x (concat $ map ((take 3).(drop b)) $ ((take 3).(drop a)) tab)
	where 
		a = 3 * (div (i-1) 3)
		b = 3 * (div (j-1) 3)

_comprobar :: Int -> [Casilla] -> Bool
_comprobar x lista = null $ filter ((x ==).sacar_valor) lista


------------------------- ENCONTRAR HUECO ANTERIOR / SIGUIENTE -------------------------

-- Encontrar la siguiente casilla no fija en el tablero
anterior_index :: (Int,Int) -> Tablero -> (Int,Int)
anterior_index (i,j) tab
	| (i,j) == (1,1) = (-1,-1)
	| j == 1 = anterior_index (i-1,10) tab
	| es_fijo (i,j-1) tab == False = (i,j-1)
	| otherwise = anterior_index (i,j-1) tab
	
-- Encontrar la anterior casilla no fija en el tablero
siguiente_index :: (Int,Int) -> Tablero -> (Int,Int)
siguiente_index (i,j) tab
	| (i,j) == (9,9) = (9,10)
	| j == 9 = siguiente_index (i+1,0) tab
	| es_fijo (i,j+1) tab == False = (i,j+1)
	| otherwise = siguiente_index (i,j+1) tab


------------------ DE "[[Int]]" A "TABLERO" Y VICEVERSA ----------------------------

-- Ejemplo de tablero 2x2: pasar de "[[0,0],[0,0]]" a "T [[[V 0, V 0],[V 0, V 0]]" y viceversa.
-- Utilizada principalmente para la lectura de tableros del fichero y salida por pantalla. 

montar :: [[Int]] -> Tablero
montar tab = T (map (map f) tab) where
	f x
		| x == 0 = V x
		| otherwise = VN x

		
desmontar :: Tablero -> [[Int]]
desmontar (T tab) = map (map _aux) tab

_aux :: Casilla -> Int
_aux (VN x) = 10*x   -- para tenerlos localizados en el print por pantalla (y ponerlos de otro color)
_aux val = sacar_valor val 


-------------------------- LEER UN TABLERO DEL .txt ----------------------------
-- Ejemplo con tableros 2x2:
-- Nos pasan la string (readFile nombre) completa con 3 tableros:
--    "[[1,1],\n[1,1]]\n\n[[2,2],\n[2,2]]\n\n[[3,3],\n[3,3]]"
-- primero con "_modificar n" donde n (1,2 o 3) es el número de tablero que queremos, devolvemos:
--    "[[2,2],[2,2]]"
-- para n = 2 por ejemplo. Y de ahí lo leemos con "read" y lo convertimos a "Tablero" con "montar".

leer_tablero :: String -> String -> Tablero 
leer_tablero n str = montar (read sudoku::[[Int]])
	where sudoku = _modificar str (read n::Int) 1

-- modificaciones de la string leída en el fichero, para poder leerla (read) correctamente
_modificar :: String -> Int -> Int -> String
_modificar (s1:s2:str) objetivo actual
	| objetivo == actual && s1 == ']' && s2 == ']' = s1:s2:[]
	| objetivo /= actual && s1 == ']' && s2 == ']' = _modificar str objetivo (actual+1)
	| objetivo /= actual || s1 == '\n' = _modificar (s2:str) objetivo actual
	| otherwise = s1 : _modificar (s2:str) objetivo actual


--------------------------- PRINT TABLERO POR PANTALLA ----------------------------------

-- La funcion únicamente juega con las strings para realizar una salida amigable del tablero por pantalla
print_tablero :: Tablero -> IO()
print_tablero tab = putStrLn (f 1 1 (show (desmontar tab)))
	where f i j (s1:s2:ss)
			| s1 == '[' && s2 == '[' = ' ' : separador2 ++ '\n' : f i j (s2:ss)
			| s1 == '[' = ' ':'|' : f i j (s2:ss)
			| s1 == ']' && s2 == ']' = " |\n " ++ separador2 
			| s1 == ']' && s2 == ',' && mod i 3 == 0 = " |\n " ++ separador2 ++ '\n' : f (i+1) 1 ss 
			| s1 == ']' && s2 == ',' = " |\n " ++ separador ++ '\n' : f (i+1) 1 ss 
			| s1 == ',' && mod j 3 == 0 = ' ':'|' : f i (j+1) (s2:ss)
			| s1 == ',' = ' ' : ' ' : f i (j+1) (s2:ss)
			| s1 == '0' = ' ' : ' ' : f i j (s2:ss)
			| s1 /= '[' && s1 /= ']' && s1 /= ',' && s2 == '0' = ' ' : s1 : f i j ss  -- aquí hacemos la marca
			| otherwise = " \ESC[34m" ++ s1 : "\ESC[0m" ++ f i j (s2:ss)
			where 
				separador = '|' : (concat $ take 3 $ repeat ((take 11 $ repeat ' ') ++ "|"))
				separador2 = '+' : (concat $ take 3 $ repeat ((take 11 $ repeat '-') ++ "+"))
				

--------------- PEQUEÑAS FUNCIONES DE AYUDA ---------------

-- Útil para coger la casilla (i,j) del tablero
get :: (Int,Int) -> Tablero -> Casilla
get (i,j) (T tab) = (tab !! (i-1)) !! (j-1) 


-- Para "Temp", devuelve la longitud de la lista de posibilidades
-- Para "V" / "VN", devuelve 0 --> también servirá para identificar de que tipo es una casilla (si longitud == 0 entonces...)
longitud :: Casilla -> Int
longitud (Temp xs) = length xs
longitud _ = 0


-- Ver si la posición (i,j) está fija o no
es_fijo :: (Int,Int) -> Tablero -> Bool
es_fijo (i,j) tab = _es_fijo (get (i,j) tab)

_es_fijo :: Casilla -> Bool
_es_fijo (VN _) = True
_es_fijo _ = False


-- Te devuelve una lista de los valores de la casilla. Si tiene algún valor ya, te devuelve una lista 
-- con dicho valor, de lo contrario si es de tipo "Temp" te devuelve todas sus posibilidades.
sacar_valores :: Casilla -> [Int]
sacar_valores (V x) = [x]
sacar_valores (VN x) = [x]
sacar_valores (Temp posibles) = posibles


-- Valor real de la casilla
sacar_valor :: Casilla -> Int
sacar_valor (V x) = x
sacar_valor (VN x) = x
sacar_valor (Temp _) = 0  


-- A raíz de la lista de posibilidades actual, debido a algún cambio en el tablero comprobar si es 
-- posible de descartarse de alguno de ellos.
actualizar_posibilidades :: [Int] -> (Int,Int) -> Tablero -> [Int]
actualizar_posibilidades [] _ _  = []
actualizar_posibilidades (x:xs) (i,j) tab
	| valido x (i,j) tab = x : actualizar_posibilidades xs (i,j) tab
	| otherwise = actualizar_posibilidades xs (i,j) tab
	

-- Ejercer una función "f" sobre todas las casillas del tablero vacias
-- Se utilizará para resetear todas a "Temp []" en algún momento.
mapCasillasVacias :: (Casilla -> Casilla) -> Tablero -> Tablero
mapCasillasVacias f (T tab) = T (map (mapIf esta_vacio f) tab)


-- mapear con "f" todas las casillas de una fila si cumplen "condicion", si no se dejan como estan
mapIf :: (Casilla -> Bool) -> (Casilla -> Casilla) -> [Casilla] -> [Casilla]
mapIf condicion f (x:xs)
	| condicion x = f x : mapIf condicion f xs
	| otherwise = x : mapIf condicion f xs
mapIf _ _ _ = []