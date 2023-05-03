-- antigui nombre iap1-tp
module Solucion where

-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

-- Ejercicio 1

-- verifica un elemento pertenece a una secuencia
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece e (l:ls) 
    | e == l = True
    | otherwise = pertenece e ls

-- funcion auxiliar
-- aplana la lista de usuarios en una lista de strings
-- sin repetidos
proyectarTodosNombres :: [Usuario] -> [String]
proyectarTodosNombres [] = []
proyectarTodosNombres (x:xs) = [nombreDeUsuario(x)] ++ proyectarTodosNombres xs

-- funcion auxiliar 
-- quita los repetidos de una lista de strings
quitarRepetidos :: (Eq t) => [t] -> [t]
quitarRepetidos [] = []
quitarRepetidos (x:xs) 
    | pertenece x xs = quitarRepetidos xs
    | otherwise = x : quitarRepetidos xs 

-- genera una lista de strings a partir de una 
-- lista de usuarios
proyectarNombres :: [Usuario] -> [String]
proyectarNombres us = 
    quitarRepetidos (proyectarTodosNombres us)

-- devuelve como resultado todos los nombres de los usuarios de la red
-- en forma de lista de strings
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios redSocial = 
    proyectarNombres(usuarios(redSocial))

-- caso de testeo
us1 = (1, "Juan")
us2 = (2, "Paulina")
us3 = (3, "Juan")
red1 = 
    ([us1, us2, us3], 
     [],
     [])

-- pasar a archivo de test
nombresDeUsuariosTest :: [String]
nombresDeUsuariosTest = nombresDeUsuariosTest red1

-- Ejercicio 2

-- funcion auxiliar
-- a partir de una lista de relaciones da una lista de usuarios que tienen 
-- una relacion con el usuario que se da como parametro
amigosDesdeRelDe :: [Relacion] -> Usuario -> [Usuario]
amigosDesdeRelDe [] _ = []
amigosDesdeRelDe (((id1, name1), (id2, name2)):rels) (id, name) 
    | id1 == id = (id2, name2) : amigosDesdeRelDe rels (id, name)
    | id2 == id = (id1, name1) : amigosDesdeRelDe rels (id, name)
    | otherwise = amigosDesdeRelDe rels (id, name)

-- devuelve todos los amigos de un usuario como lista de usuarios
-- a partir de una red social y un usuario
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe redSocial us = amigosDesdeRelDe (relaciones(redSocial)) us

-- caso de testeo
us4 = (4, "Maria")
rel3_4 = (us3, us4)
rel4_1 = (us4, us1)
rel2_1 = (us2, us1)
red2 = 
    ([us1, us2, us3, us4],
     [rel3_4, rel4_1, rel2_1],
     [])

-- pasar a archivo de test
amigosDeTest :: [Usuario] 
amigosDeTest = amigosDe red2 us1

-- Ejercicio 3

-- devuelve el numero de amigos que tiene un usuario en la red social
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos redSocial us = length (amigosDe redSocial us)

-- caso de testeo
red3 = red2

-- pasar a archivo de test
cantidadDeAmigosTest :: Int
cantidadDeAmigosDeTest = cantidadDeAmigos red3 us1

-- Ejercicio 4

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos = undefined

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined
