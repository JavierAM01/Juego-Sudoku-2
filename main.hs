module Main (main) where 

import ES



main :: IO ()
main = do 
	putStrLn "\nElegir modo de juego:"
	putStrLn " (1) Jugar. "
	putStrLn " (2) Resolver un sudoku."
	putStrLn " (3) Continuar partida guardada."
	putStr " > "
	modo <- getLine
	if modo == "3" then jugar_partida_guardada else jugar_predeterminado modo
	putStrLn "\nVolver a jugar? (y/n)"
	putStr " > "
	res <- getLine
	if res == "y" then main else putStrLn "Saliendo..."
	
