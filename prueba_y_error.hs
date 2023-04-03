module Prueba_y_Error (prueba_y_error) where

import Funciones


------------------------- RESOLVER EL SUDOKU -------------------------

-- Con "_resolver" hacemos todo el bucle y "resolver" le llama con el valor
-- x = 1, para probarlo en la primera casilla (1,1) y de ahí vamos probando en 
-- la siguiente casilla de forma inductiva, si no vale probamos con (x+1) y si no vale ninguno (1..9)
-- entonces volvemos para atrás y con el siguiente.

prueba_y_error :: Tablero -> Tablero
prueba_y_error tab = _resolver 1 (1,1) tab

_resolver :: Int -> (Int,Int) -> Tablero -> Tablero
_resolver x (i,j) tab
	| (i,j) == (9,10) = tab 
	| x == 10 && (ai,aj) == (-1,-1) = tab  -- -> No existe solución !
	| es_fijo (i,j) tab = _resolver x (si,sj) tab
	| x == 10 = _resolver (1 + ax) (ai,aj) (insertar 0 (ai,aj) tab)
	| valido x (i,j) tab = _resolver 1 (si,sj) (insertar x (i,j) tab)
	| otherwise = _resolver (x+1) (i,j) tab
	where 
		(ai,aj) = anterior_index (i,j) tab
		(si,sj) = siguiente_index (i,j) tab
		ax = head $ sacar_valores (get (ai,aj) tab)



