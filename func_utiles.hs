-- funciones posiblemente utiles o cosas q pense q servian y descarte

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece e (l:ls) 
    | e == l = True
    | otherwise = pertenece e ls

len :: [t] -> Int
len [] = 0
len (t:ts) = 1 + len ts

-- funcion auxiliar 
cantidadDeApariciones :: (Eq t) => t -> [t] -> Integer
cantidadDeApariciones x l
  | length l == 0 = 0
  | head l == x = 1 + restoLista
  | otherwise = restoLista
  where
    restoLista = cantidadDeApariciones x (tail l) 

-- funcion auxiliar
quitar :: (Eq t) => t -> [t] -> [t]
quitar x xs
    | not (pertenece x xs) = xs
    | xs == [] = xs
    | x == head xs = tail xs
    | otherwise = [x] ++ quitar x (tail xs)

-- funcion auxiliar
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos x (y:ys)
    | x == y = quitarTodos x (quitar x (y:ys))
    | otherwise = [y] ++ quitarTodos x ys

-- verifica que ambas listas tengan los mismos elementos
-- sin importar el orden
-- vinculada a: 
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] _ = True
mismosElementos (x:xs) y 
    | not (pertenece x y) = False
    | aparicionesIguales = mismosElementos listaModX listaModY
    where
        aparicionesIguales = 
            cantidadDeApariciones x y == cantidadDeApariciones x (x:xs)
        listaModX = quitarTodos x xs
        listaModY = quitarTodos x y
-- verificada con: 
-- "boka" "kbao"
-- "boka" "boke" 
