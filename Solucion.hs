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
-- de una lista de usuarios genera una lista de strings
-- con repetidos
proyectarTodosNombres :: [Usuario] -> [String]
proyectarTodosNombres [] = []
proyectarTodosNombres (x:xs) = [nombreDeUsuario(x)] ++ proyectarTodosNombres xs

-- funcion auxiliar 
-- quita los repetidos de una lista
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

{-- caso de testeo

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
--}

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

{-- caso de testeo

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
--}



-- Ejercicio 3

-- devuelve el numero de amigos que tiene un usuario en la red social
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos redSocial us = length (amigosDe redSocial us)

{-- caso de testeo
red3 = red2
-- pasar a archivo de test

cantidadDeAmigosTest :: Int
cantidadDeAmigosDeTest = cantidadDeAmigos red3 us1
--}



-- Ejercicio 4

-- FUNCION AUXILIAR
--compararUsuarios recibe la red social (solo para tener la cantidad de amigos de cada usuario), la lista de todos los
-- usuarios y el primero de esta lista, a partir de ahi compara la cantidad de amigos de todos los usuarios de la lista
--uno a uno empezando con el primero, si un usuario tiene mas amigos que el primero se vuelve a llamar a la funcion
--con este usuario nuevo y con la cola de la lista
compararUsuarios :: RedSocial -> [Usuario] -> Usuario -> Usuario
compararUsuarios red [] x = x
compararUsuarios red us x | cantidadDeAmigos red x >= cantidadDeAmigos red (head (us)) = compararUsuarios red (tail (us)) x 
                          | otherwise = compararUsuarios red (tail us) (head us)

-- Llama a la funcion compararUsuarios pasandole como parametros la red social la lista de todos los usuarios de la res
--y el primer usuario de esta lista
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = compararUsuarios red (usuarios red) (head (usuarios red))



-- Ejercicio 5
-- Se fija si el usuario con mas amigos de la red tiene mas de un millon de amigos, si los tiene es verdadero
-- si no, sabemos que no hay otro que cumpla esto
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red | cantidadDeAmigos red (usuarioConMasAmigos(red)) > 1000000 = True
                      | otherwise = False



--Ejercicio 6

-- FUNCION AUXILIAR
-- Devuelve True si el usuario de la publicacion es el mismo que del que queremos su lista de publicaciones
--si no lo es devuelve False
esPublicacionDe :: Usuario -> Publicacion -> Bool
esPublicacionDe us p | usuarioDePublicacion p == us = True
                     | otherwise = False

--FUNCION AUXILIAR
-- A esta funcion se le pasan las publicaciones de la red y el usuario del que queremos 
-- sus publicaciones y toma cada publicacion de la lista y la pasa a esPublicacionDe para chequear
-- si la publicacion es del Usuario, si lo es la agrega a la lista y se chequea la siguiente invocando la cola de la lista,
-- sino se toma la cola de la lista para chequear el siguiente asi hasta que la lista este vacia
filtrarPublicaciones :: [Publicacion] -> Usuario -> [Publicacion]
filtrarPublicaciones [] us = []
filtrarPublicaciones ps us | esPublicacionDe us (head (ps)) = (head (ps)) : filtrarPublicaciones (tail ps) us
                           | otherwise = filtrarPublicaciones (tail ps) us

-- Llama a la funcion filtrarPublicaciones pasandole como parametros las publicaciones de la red y el usuario del que 
-- queremos  sus publicaciones
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red us = filtrarPublicaciones (publicaciones (red)) us



-- Ejercicio 7

--FUNCION AUXILIAR
-- Si el usuario pertenece a la lista del tercer elemento de la tripla publicacion significa que le dio me gusta
-- por eso solo se cheque si el usuario pertenece a esta lista
leDioLike :: Publicacion -> Usuario -> Bool
leDioLike (a, b, l) us | pertenece us l = True
                       | otherwise = False

-- FUNCION AUXILIAR
-- Recibiendo el usuario como parametro y la lista de publicacione, pasamos las publicaciones de a una y vemos si nuestro
-- usuario le dio me gusta (es decir si aparece en la lista de usuarios perteneciente a la publicacion), si le gusta la
-- publicacion esta se agrega a la lista y se pasa con la siguiente hasta que quede vacia
filtrarPorLikes :: [Publicacion] -> Usuario -> [Publicacion]
filtrarPorLikes [] us = []
filtrarPorLikes ps us | leDioLike (head ps) us = head ps : filtrarPorLikes (tail ps) us
                      | otherwise = filtrarPorLikes (tail ps) us 

-- Igual que en el ejercicio anterior se pasan los parametros a una funcion filtrarPorLikes, que en este caso como dice el
-- nombre se seleccionan segun si le dio me gusta el usuario dado
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red us = filtrarPorLikes (publicaciones (red)) us



--Ejercicio 8

-- La funcion se fija si las publicaciones que le gustan al usuario 1 son las mismas que le gustan al usuario 2
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red us1 us2 | publicacionesQueLeGustanA red us1 == publicacionesQueLeGustanA red us2 = True
                                            | otherwise = False


--Ejercicio 9
--auxiliar: cuenta el numero de likes que un usuario dió dentro de una lista de publicaciones
cuentalikes :: RedSocial -> Usuario -> [Publicacion] -> Int
cuentalikes red us t
    | length t == 1 && (leDioLike (head t) us) == True = 1
    | length t == 1 && (leDioLike (head t) us) == False = 0
    | leDioLike (head t) us == True = 1 + cuentalikes red us (tail t)
    | leDioLike (head t) us == False = 0 + cuentalikes red us (tail t)
    
--auxiliar: cuenta el numero de likes que un usuario2 le dio a la lista de publicaciones de otro usuario1
likesPersonales :: RedSocial -> Usuario -> Usuario -> Int
likesPersonales red us1 us2 = cuentalikes red us2 (publicacionesDe red us1)

--auxiliar:  si el numero de likes personales de un us2 es igual al numero de publicaciones de un us1 devuelve True 
--revisa si us2 es seguidor fiel de us1
esseguidorfiel :: RedSocial -> Usuario -> Usuario -> Bool
esseguidorfiel red us1 us2
    |likesPersonales red us1 us2 == length(publicacionesDe red us1) = True
    |likesPersonales red us1 us2 < length(publicacionesDe red us1) = False
    
 --aux: si hay un seguidor fiel de un usuario dentro de una lista
hayseguidorfiel :: RedSocial -> Usuario -> [Usuario] -> Bool 
hayseguidorfiel red us t
    |length t == 1 && esseguidorfiel red us (head t) == True = True
    |length t == 1 && esseguidorfiel red us (head t) == False = False
    |esseguidorfiel red us (head t) == False = hayseguidorfiel red us (tail t)
    |esseguidorfiel red us (head t) == True = True
    
 

-- True si hay uno o mas usuarios que hayan dado like a todas las publicaciones del usuario
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red us = hayseguidorfiel red us (likesDePublicacion (head(publicacionesDe red us)))
    



--Ejercicio 10
-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined
