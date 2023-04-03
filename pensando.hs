module Pensando (pensando, buscar_soluciones, paso1, paso2) where

import Funciones




-------------------------------------------------------------------------------------------
------------------------------ Posibles multiples soluciones ------------------------------
-------------------------------------------------------------------------------------------


{--
	buscar_soluciones:
		Función que se usa después de haber "inspeccionado" el sudoku previamente e 
		intertar solucinarlo lo máximo posible con "pensando".
		Busca una casilla determinada y prueba uno a uno con sus valores posibles a 
		resolver el sudoku (en caso de haber +1 solución las encuentra), para esto
		último llamamos a "probar_soluciones".
		
	s (default = 2) : 
		Longitud del nº de posibilidades de la casilla a buscar. Empezamos con 2 
		(no puede existir ninguna con 1 porque la habríamos introducido antes), con esto
		tenemos un (100/s)% de acierto. Si no encontramos -> s+1.
	
	n (default = 0) : 
		Índice de 0 a 81 para buescar en todas la casillas.
		
	probar_soluciones:
		Prueba soluciones y sigue probando con los demás valores. Si es válida 
		la añade a la lista solución.
--}

buscar_soluciones :: Int -> Int -> Tablero -> [Tablero]
buscar_soluciones s n tab    -- s = "longitud de la casilla", s = 2 -> 50% de acierto, -- n = contador
	| n == 81 = buscar_soluciones (s+1) 0 tab
	| es_fijo (i,j) tab = buscar_soluciones s (n+1) tab
	| longitud casilla == s = probar_soluciones (i,j) posibles tab 
	| otherwise = buscar_soluciones s (n+1) tab
	where 
		(i,j) = (1 + (div n 9), 1 + (mod n 9))
		casilla = get (i,j) tab
		posibles = sacar_valores casilla
	
probar_soluciones :: (Int,Int) -> [Int] -> Tablero -> [Tablero]
probar_soluciones _ [] _ = []
probar_soluciones (i,j) (x:xs) tab
	| end_game nuevo_tab = nuevo_tab : probar_soluciones (i,j) xs tab
	| no_existe_solucion nuevo_tab = probar_soluciones (i,j) xs tab
	| otherwise = (buscar_soluciones 2 0 nuevo_tab) ++ probar_soluciones (i,j) xs tab
	where nuevo_tab = _pensando (insertar x (i,j) tab)


-------------------------------------------------------------------------------------------
-- Resolver el tablero con reglas. Solo introducimos valor si este es en ÚNICO posible. Dos partes:
--   * paso1) Hacer una primera ronda por todo el tablero poniendo los posibles valores en cada casilla.
--   * paso2) Paso en bucle hasta solucionar el sudoku. Probará con dos procesos: fase1 y fase2.
-------------------------------------------------------------------------------------------

pensando :: Tablero -> Tablero 
pensando tab = _pensando (paso1 0 tab)


_pensando :: Tablero -> Tablero 
_pensando tab
	| end_game nuevo_tab = nuevo_tab
	| tab /= nuevo_tab = _pensando nuevo_tab
	| otherwise = tab
	where nuevo_tab = paso2 tab


paso1 :: Int -> Tablero -> Tablero
paso1 n tab
	| n == 81 = tab
	| sacar_valores casilla /= [0] = paso1 (n+1) tab
	| otherwise = paso1 (n+1) nuevo_tab
	where 
		(i,j) = (1 + (div n 9), 1 + (mod n 9))
		casilla = get (i,j) tab
		x = Temp (actualizar_posibilidades [1..9] (i,j) tab)
		nuevo_tab = insertar_valor x (i,j) tab


paso2 :: Tablero -> Tablero
paso2 tab 
	| tab /= nuevo_tab = nuevo_tab
	| otherwise = fase2 0 tab
	where nuevo_tab = fase1 0 tab
	
-------------------------------------------------------------------------------------------
-- Dos fases: la 1ª se realiza por defecto, en caso de no encontrar ningún cambio entonces 
-- acudimos a la 2ª.
--   FASE 1) Recorre casilla a casilla y observa:
--    - Si ya hay un valor, entonces continuamos.
--    - Si el número de posibilidades es 1, entonces la introducimos directamente y seguimos.
--    - Si no, actualiza las posibilidades (vuelve a comprobar si sólo hay una) y mira a ver 
--   si es el único valor posible en su fila / columna / bloque con la función "comprobar_fcb".
--   FASE 2) Movimientos no tan directos.
-------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------
-------------------------------------- FASE 1 ---------------------------------------------
-------------------------------------------------------------------------------------------

fase1 :: Int -> Tablero -> Tablero
fase1 n tab
	| n == 81 = tab
	| longitud casilla == 0 = fase1 (n+1) tab
	| longitud casilla == 1 = fase1 (n+1) (insertar (head $ sacar_valores casilla) (i,j) tab)
	| length nuevas_posibilidades == 1 = fase1 (n+1) (insertar (head nuevas_posibilidades) (i,j) tab)
	| otherwise = fase1 (n+1) nuevo_tab
	where 
		(i,j) = (1 + (div n 9), 1 + (mod n 9))
		casilla = get (i,j) tab
		nuevas_posibilidades = actualizar_posibilidades (sacar_valores casilla) (i,j) tab
		temp_tab = insertar_valor (Temp nuevas_posibilidades) (i,j) tab 
		temp_valor = comprobar_fcb nuevas_posibilidades (i,j) tab
		nuevo_tab
			| temp_valor == -1 = temp_tab
			| otherwise = insertar temp_valor (i,j) tab


-- ver si es el único valor posible en una fila / columna / bloque (fcb)
comprobar_fcb :: [Int] -> (Int,Int) -> Tablero -> Int
comprobar_fcb [] _ _ = -1
comprobar_fcb (x:xs) (i,j) tab
	| unicidad = x
	| otherwise = comprobar_fcb xs (i,j) tab
	where unicidad = _es_unico x (i,j) tab


{-
  Se va a hacer un bucle por toda la fila, luego la columna y finalmente por el bloque:
    1) _unico_en_fila, 
    2) _unico_en_columna, 
    3) _unico_en_bloque,
  para ver si "x" es el único valor que pueda estar en uno de esos tres. De ser así entonces ese valor 
  debe ser introducido en el Tablero.
  
  La función auxiliar "_esta_aqui" nos dirá si "x" esta es la casilla que le preguntemos.
  
  La función auxiliar (de orden superior) "map_grupo" nos ayuda con el proceso de de ver si es 
  el único. Para ello le pasamos una función "f" que a partir del índice original (i,j) y el iterador
  actual, nos devuelve la posición del tablero en la que estamos actualmente.
-}

_es_unico :: Int -> (Int,Int) -> Tablero -> Bool
_es_unico x (i,j) tab = _unico_en_fila x (i,j) tab 
						|| _unico_en_columna x (i,j) tab 
							|| _unico_en_bloque x (i,j) tab


_unico_en_fila :: Int -> (Int,Int) -> Tablero -> Bool
_unico_en_fila x (i,j) tab = map_grupo f x (i,j) 1 tab
	where f (fila,col) actual = (i,actual)


_unico_en_columna :: Int -> (Int,Int) -> Tablero -> Bool
_unico_en_columna x (i,j) tab = map_grupo f x (i,j) 1 tab
	where f (fila,col) actual = (actual,j)


_unico_en_bloque :: Int -> (Int,Int) -> Tablero -> Bool
_unico_en_bloque x (i,j) tab = map_grupo f x (i,j) 1 tab
	where f (fila,col) actual = (ia,ja)
		where 
			(i0,j0) = (1 + 3*(div (fila-1) 3), 1 + 3*(div (col-1) 3))
			(ia,ja) = (i0 + (div (actual-1) 3), j0 + (mod (actual-1) 3))


map_grupo :: ((Int,Int) -> Int -> (Int,Int)) -> Int -> (Int,Int) -> Int -> Tablero -> Bool
map_grupo f x (i,j) actual tab
	| actual == 10 = True
	| (ia,ja) == (i,j) = map_grupo f x (i,j) (actual+1) tab
	| _esta_aqui x (get (ia,ja) tab) = False
	| otherwise = map_grupo f x (i,j) (actual+1) tab
	where (ia,ja) = f (i,j) actual


_esta_aqui :: Int -> Casilla -> Bool
_esta_aqui x casilla = not $ null (filter (== x) (sacar_valores casilla))



-------------------------------------------------------------------------------------------
-------------------------------------- FASE 2 ---------------------------------------------
-------------------------------------------------------------------------------------------
{-
   "fase2": Se trata de cambios no tan directos, utiliza las siguientes ideas:
   
	Idea 1) "_buscar_pares": 
		se trata de ver si hay algún par (a,b) tal que dos casillas de una
		fila / columna / bloque, los tengan como únicas opciones. De ser así estamos 100% seguros
		que en dichas casillas una tiene q tener el "a" y la otra el "b". Aunque no sepamos todavía 
		donde va exactamente "a" y "b", ya sabemos que en la demás casillas de la fila / columna / bloque
		no pueden ir, así quitamos "a" y "b" de las opciones de las demás casillas de la 
		fila / columna / bloque correspondiente.
		
	Idea 2) "_unico_en_bloque_lineal": 
		comprueba si cierto número se puede poner sólo en la fila / columna de un bloque. Entonces 
		quitaríamos ese número de las opciones de las otras dos filas / columnas de ese bloque.
		 * Ejemplo: sea la fila, f = [v1,v2,v3,v4,v5,v6,v7,v8,v9], entonces si el número "x" solo puede estar en	
		 [v1,v2,v3] entonces 100% tiene que ser: v1=x, v2=x ó v3=x. Por lo que las otras dos filas del bloque 
		 de [v1,v2,v3] no pueden tener el valor x (lo mismo con las columnas).
-}

fase2 :: Int -> Tablero -> Tablero
fase2 n tab
	| n == 81 = tab
	| longitud casilla == 0 = fase2 (n+1) tab
	| longitud casilla == 2 = fase2 (n+1) nuevo_tab_2
	| otherwise = fase2 (n+1) nuevo_tab
	where 
		(i,j) = (1 + (div n 9), 1 + (mod n 9))
		casilla = get (i,j) tab
		posibles = actualizar_posibilidades (sacar_valores casilla) (i,j) tab
		nuevo_tab = _unico_en_bloque_lineal (i,j) posibles tab
		nuevo_tab_2 = _buscar_pares (i,j) (1,1) nuevo_tab


-- Idea 2)
_unico_en_bloque_lineal :: (Int,Int) -> [Int] -> Tablero -> Tablero
_unico_en_bloque_lineal _ [] tab = tab
_unico_en_bloque_lineal (i,j) (x:xs) tab = _unico_en_bloque_lineal (i,j) xs nuevo_tab
	where nuevo_tab = _unico_en_bloque_fila x (i,j) 1 tab
	

_unico_en_bloque_fila :: Int -> (Int,Int) -> Int -> Tablero -> Tablero
_unico_en_bloque_fila x (i,j) actual tab
	| actual == 10 = _unico_en_bloque_columna x (i,j) 1 nuevo_tab
	| div (j-1) 3 == div (actual-1) 3 = _unico_en_bloque_fila x (i,j) (actual+1) tab
	| null (filter (== x) valores) = _unico_en_bloque_fila x (i,j) (actual+1) tab
	| otherwise = _unico_en_bloque_columna x (i,j) 1 tab
	where 
		casilla = get (i,actual) tab
		valores = sacar_valores casilla
		nuevo_tab = eliminar_2filas_bloque x (i,j) tab


_unico_en_bloque_columna :: Int -> (Int,Int) -> Int -> Tablero -> Tablero
_unico_en_bloque_columna x (i,j) actual tab
	| actual == 10 = nuevo_tab
	| div (i-1) 3 == div (actual-1) 3 = _unico_en_bloque_columna x (i,j) (actual+1) tab
	| null (filter (== x) valores) = _unico_en_bloque_columna x (i,j) (actual+1) tab
	| otherwise = tab
	where 
		casilla = get (actual,j) tab
		valores = sacar_valores casilla
		nuevo_tab = eliminar_2columnas_bloque x (i,j) tab
		
		
eliminar_2filas_bloque :: Int -> (Int,Int) -> Tablero -> Tablero
eliminar_2filas_bloque x (i,j) tab = eliminar_2_tiras_bloque fst x (i,j) 0 tab


eliminar_2columnas_bloque :: Int -> (Int,Int) -> Tablero -> Tablero
eliminar_2columnas_bloque x (i,j) tab = eliminar_2_tiras_bloque snd x (i,j) 0 tab


-- Eliminar x de las posibilidades de las 2 tiras (filas / columanas) restantes del bloque
-- (orden superior) la función "f" indica si fijarse en la fila o la columna
eliminar_2_tiras_bloque :: ((Int,Int) -> Int) -> Int -> (Int,Int) -> Int -> Tablero -> Tablero
eliminar_2_tiras_bloque f x (i,j) actual tab
	| actual == 9 = tab
	| f (ia,ja) == f (i,j) || longitud casilla == 0 = eliminar_2_tiras_bloque f x (i,j) (actual+1) tab
	| otherwise = eliminar_2_tiras_bloque f x (i,j) (actual+1) nuevo_tab
	where
		(i0,j0) = (1 + 3*(div (i-1) 3), 1 + 3*(div (j-1) 3))
		(ia,ja) = (i0 + (div actual 3), j0 + (mod actual 3))
		casilla = get (ia,ja) tab
		nuevo_tab = eliminar_valor x (ia,ja) tab


-- Idea 1) 
_buscar_pares :: (Int,Int) -> (Int,Int) -> Tablero -> Tablero
_buscar_pares (i,j) (tipo,actual) tab
	| actual == 10 && tipo == 2 = tab
	| actual == 10 = _buscar_pares (i,j) (2,1) tab
	| (ia,ja) == (i,j) = _buscar_pares (i,j) (tipo,actual+1) tab
	| longitud casilla == 0 = _buscar_pares (i,j) (tipo,actual+1) tab
	| casilla == (get (i,j) tab) = _buscar_pares (i,j) (tipo,10) nuevo_tab
	| otherwise = _buscar_pares (i,j) (tipo,actual+1) tab
	where 
		(ia,ja) = if tipo == 1 then (i,actual) else (actual,j)
		casilla = get (ia,ja) tab
		nuevo_tab = if tipo == 1 then _eliminar_pares_fila i (j,ja) 1 tab 
					else _eliminar_pares_columna j (i,ia) 1 tab


_eliminar_pares_fila :: Int -> (Int,Int) -> Int -> Tablero -> Tablero
_eliminar_pares_fila fila (c1,c2) actual tab
	| actual == 10 = tab
	| actual == c1 || actual == c2 = _eliminar_pares_fila fila (c1,c2) (actual+1) tab
	| longitud (get (fila,actual) tab) == 0 = _eliminar_pares_fila fila (c1,c2) (actual+1) tab
	| otherwise = _eliminar_pares_fila fila (c1,c2) (actual+1) nuevo_tab
	where 
		[x1, x2] = sacar_valores (get (fila,c1) tab)
		tab_temp = eliminar_valor x1 (fila,actual) tab
		nuevo_tab = eliminar_valor x2 (fila,actual) tab_temp


_eliminar_pares_columna :: Int -> (Int,Int) -> Int -> Tablero -> Tablero
_eliminar_pares_columna col (f1,f2) actual tab
	| actual == 10 = tab
	| actual == f1 || actual == f2 = _eliminar_pares_columna col (f1,f2) (actual+1) tab
	| longitud (get (actual,col) tab) == 0 = _eliminar_pares_columna col (f1,f2) (actual+1) tab
	| otherwise = _eliminar_pares_columna col (f1,f2) (actual+1) nuevo_tab
	where 
		[x1, x2] = sacar_valores (get (f1,col) tab)
		tab_temp = eliminar_valor x1 (actual,col) tab
		nuevo_tab = eliminar_valor x2 (actual,col) tab_temp
		

