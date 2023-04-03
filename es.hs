module ES (jugar_predeterminado, jugar_partida_guardada) where 

import Funciones
import Prueba_y_Error
import Pensando



main_path :: String
main_path = "D:/Haskell/Sudoku"


-- En este script todas las funciones se explican con su propio nombre.
-- Se encargan de la entrada y salida por pantalla, dar opciones de niveles,
-- guardar partida, etc.  

jugar_predeterminado :: String -> IO()
jugar_predeterminado modo = do 
	putStrLn "\nNivel del sudoku:"
   	putStrLn " (1) Facil."
	putStrLn " (2) Medio."
	putStrLn " (3) Dificil."
	putStr " > "
	nivel <- getLine
	let path_nivel
			| nivel == "1" = "facil"
			| nivel == "2" = "medio"
			| otherwise = "dificil"
	putStrLn "\nElige uno:"
	putStrLn (" (1) " ++ _f_ 1 path_nivel)
	putStrLn (" (2) " ++ _f_ 2 path_nivel)
	putStrLn (" (3) " ++ _f_ 3 path_nivel)
	putStr " > "
	n <- getLine 
	str_sudokus <- readFile (main_path ++ "Tableros/predeterminados/" ++ path_nivel ++ ".txt")
	let tab = leer_tablero n str_sudokus
	if modo == "1" then jugar (mapCasillasVacias (\ _ -> Temp []) tab) else resolver tab    -- jugar (paso1 0 tab)


_f_ :: Int -> String -> String
_f_ n nivel 
	| nivel == "facil" = ["Muy facil","Facil","Moderado"]                !! (n-1)
	| nivel == "medio" = ["Dificil","Muy dificil","Super dificil"]       !! (n-1)
	| otherwise        = ["Hiper dificil","Extremo","Solucion multiple"] !! (n-1)


jugar_partida_guardada:: IO()
jugar_partida_guardada = do
	putStrLn "Nombre del fichero:"
	putStr " > "
	nombre <- getLine
	ss <- readFile (main_path ++ "Tableros/guardados/" ++ nombre ++ ".txt")
	let tab = read ss::Tablero
	jugar tab


jugar :: Tablero -> IO()
jugar tab = do
	putStrLn "\nTablero: "
	print_tablero tab
	if end_game tab then putStrLn "Sudoku completado!" 
	else do
		putStrLn "\nOpcion:"
		putStrLn " 1) Poner numero"
		putStrLn " 2) Borrar numero"
		putStrLn " 3) Ver posibilidades"
		putStrLn " 4) Introducir posibilidades"
		putStrLn " 5) Salir"
		putStr " > "
		opcion <- getLine
		if opcion == "1" then introducir_valor tab 
		else if opcion == "2" then borrar tab
		else if opcion == "3" then ver_posibilidades tab 
		else if opcion == "4" then introducir_posibilidades tab 
		else salir tab


resolver :: Tablero -> IO ()
resolver tab = do 
	putStrLn "\nTablero:"
	print_tablero tab
	putStrLn "\nResolver con:"
	putStrLn " 1) Prueba y Error"
	putStrLn " 2) Pensando"
	putStr " > "
	metodo <- getLine
	if metodo == "1" then do 
		let tab_resuelto = prueba_y_error tab
		if end_game tab_resuelto then do
			putStrLn "\nTablero resuelto:"
			print_tablero tab_resuelto
		else putStrLn "\nNO existe solucion!"
	else resolver_pensando tab
	

resolver_pensando :: Tablero -> IO ()
resolver_pensando tab = do
	let tab2 = pensando tab
	if end_game tab2 then do 
		putStrLn "\nSolucion:"
		print_tablero tab2
	else do 
		let tableros = buscar_soluciones 2 0 tab2
		    n = length tableros
		if n == 1 then do
			putStrLn "\nSolucion:"
			print_tablero (head tableros)
		else do
			putStrLn ("\nSe han encontrado " ++ (show n) ++ " soluciones !!")
			print_soluciones 1 tableros


print_soluciones :: Int -> [Tablero] -> IO ()
print_soluciones _ [] = putStrLn "\nNo hay mas soluciones!"
print_soluciones i (t:ts) = do 
	putStrLn ("\nSolucion " ++ (show i) ++ ":")
	print_tablero t
	putStrLn "\nVer mas soluciones (y/n):"
	putStr " > "
	opc <- getLine
	if opc == "y" then print_soluciones (i+1) ts else putStrLn ""


introducir_valor :: Tablero -> IO ()
introducir_valor tab = do
	putStrLn "\nModificar casilla: "
	(i,j) <- pedir_casilla_a_modificar tab
	putStrLn "\nValor a introducir: "
	putStr " > "
	x_str <- getLine
	let 
		x = read x_str::Int
		es_valido = valido x (i,j) tab
		new_tab = insertar x (i,j) tab
	if es_valido then jugar new_tab else do 
		putStrLn "Numero NO valido!!"
		introducir_valor tab
		

ver_posibilidades :: Tablero -> IO ()
ver_posibilidades tab = do
	putStrLn "\nCasilla: "
	(i,j) <- pedir_casilla_a_modificar tab
	let 
		casilla = get (i,j) tab
		posibilidades = sacar_valores casilla
	putStrLn (show posibilidades)
	jugar tab
	
	
introducir_posibilidades :: Tablero -> IO()
introducir_posibilidades tab = do
	putStrLn "\nCasilla: "
	(i,j) <- pedir_casilla_a_modificar tab
	let 
		casilla = get (i,j) tab
		posibilidades = sacar_valores casilla
	putStrLn ("\nValores actuales:" ++ show posibilidades)
	putStr "Anadir nuevos: "
	nuevos <- getLine
	let 
		nuevos_lista = read ('[' : nuevos ++ "]")::[Int]
		xs = juntar nuevos_lista (reverse posibilidades)
		nuevo_tab = insertar_valor (Temp xs) (i,j) tab
	putStrLn ("Actualizados: " ++ show xs)
	jugar nuevo_tab
	

juntar :: [Int] -> [Int] -> [Int]
juntar (x:xs) ys
	| x < 1 || x > 9 = juntar xs ys
	| null (filter (== x) ys) = juntar xs (x:ys)
	| otherwise = juntar xs ys
juntar [] ys = reverse ys
	
		
borrar :: Tablero -> IO()
borrar tab = do 
	putStrLn "\nBorrar casilla: "
	(i,j) <- pedir_casilla_a_modificar tab
	let new_tab = insertar 0 (i,j) tab
	jugar new_tab
	


pedir_casilla_a_modificar :: Tablero -> IO (Int,Int)
pedir_casilla_a_modificar tab = do
	putStr " - Fila: "
	i_str <- getLine
	putStr " - Columna: "
	j_str <- getLine
	let 
		i = read i_str::Int
		j = read j_str::Int
	if i<1 || j<1 || i>9 || j>9 || es_fijo (i,j) tab then do 
		putStrLn "Casilla no valida! Vuelve a intentarlo ..."
		pedir_casilla_a_modificar tab
	else do return (i,j)


salir :: Tablero -> IO()
salir tab = do 
	putStrLn "Quieres guardar la partida? (y/n):"
	putStr " > "
	res <- getLine
	if res == "y" then guardar tab else putStrLn "Partida NO guardada!"
	putStrLn "Fin de la partida..."


guardar :: Tablero -> IO()
guardar tab = do 
	putStrLn "Nombre del fichero a guardar:"
	putStr " > "
	nombre <- getLine
	writeFile (main_path ++ "Tableros/guardados/" ++ nombre ++ ".txt") (show tab)
	putStrLn "Partida guardada correctamente!"
