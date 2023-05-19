-- antigui nombre iap1-tp
module Solucion where


-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Augusto Gantes, gantesaugusto3@gmail.com, 214/22
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

-- (id, nombre)
type Usuario = (Integer, String) 
-- usuarios que se relacionan
type Relacion = (Usuario, Usuario)
-- (usuario que publica, texto publicacion, likes)
type Publicacion = (Usuario, String, [Usuario]) 
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

-- FUNCION AUXILIAR
len :: [t] -> Int
len [] = 0
len (t:ts) = 1 + len ts

-- FUNCION AUXILIAR
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece e (l:ls) 
    | e == l = True
    | otherwise = pertenece e ls

-- FUNCION AUXILIAR
proyectarTodosNombres :: [Usuario] -> [String]
proyectarTodosNombres [] = []
proyectarTodosNombres (x:xs) = [nombreDeUsuario(x)] ++ proyectarTodosNombres xs

-- FUNCION AUXILIAR 
quitarRepetidos :: (Eq t) => [t] -> [t]
quitarRepetidos [] = []
quitarRepetidos (x:xs) 
    | pertenece x xs = quitarRepetidos xs
    | otherwise = x : quitarRepetidos xs 

-- Devuelve como resultado todos los nombres de los usuarios de la red
-- en forma de lista de strings sacando los repetidos de la lista resultante
-- de proyectar los nombres de los ususarios
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios redSocial =
    quitarRepetidos (proyectarTodosNombres (usuarios(redSocial)))

-- Ejercicio 2
                                                                                
-- FUNCION AUXILIAR
amigosDesdeRelDe :: [Relacion] -> Usuario -> [Usuario]
amigosDesdeRelDe [] _ = []
amigosDesdeRelDe (((id1, name1), (id2, name2)):rels) (id, name) 
    | id1 == id = (id2, name2) : amigosDesdeRelDe rels (id, name)
    | id2 == id = (id1, name1) : amigosDesdeRelDe rels (id, name)
    | otherwise = amigosDesdeRelDe rels (id, name)

--Devuelve todos los amigos de un usuario como lista de usuarios
--a partir de una red social y un usuario utilizando amigosDesdeRelDe junto a 
--un usuario en particular, analizando si el usuario pertenece a la relacion
--utilizando la informacion de los ids
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe redSocial us = amigosDesdeRelDe (relaciones(redSocial)) us

-- Ejercicio 3

-- Devuelve el numero de amigos que tiene un usuario en la red social a partir 
-- de calcular la longitud de la lista que contiene a sus amigos
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos redSocial us = len (amigosDe redSocial us)

-- Ejercicio 4

-- FUNCION AUXILIAR
compararUsuarios :: RedSocial -> [Usuario] -> Usuario -> Usuario
compararUsuarios _ [] x = x
compararUsuarios red (u:us) x 
    | amigosX >= amigosU = compararUsuarios red us x 
    | otherwise = compararUsuarios red us u
    where 
        amigosX = cantidadDeAmigos red x
        amigosU = cantidadDeAmigos red u

--Llama a la funcion compararUsuarios pasandole como parametros la red social 
--la lista de todos los usuarios de la red y el primer usuario de esta lista,
--y ahi se comparan los usuarios uno a uno 
-- REQUIERE QUE |usuarios| > 0
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = 
    compararUsuarios red (usuarios red) (head (usuarios red))

-- Ejercicio 5

-- FUNCION AUXILIAR
usuarioConNYUnAmigos :: RedSocial -> Int -> Bool
usuarioConNYUnAmigos red n
    | cantidadDeAmigos red (usuarioConMasAmigos(red)) > n = True
    | otherwise = False

-- Se fija si el usuario con mas amigos de la red tiene mas de un millon de 
-- amigos, si los tiene es verdadero si no, sabemos que no hay otro que cumpla 
-- esto
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = usuarioConNYUnAmigos red 1000000

--Ejercicio 6

-- FUNCION AUXILIAR
esPublicacionDe :: Usuario -> Publicacion -> Bool
esPublicacionDe u p 
    | usuarioDePublicacion p == u = True
    | otherwise = False

-- FUNCION AUXILIAR
filtrarPublicaciones :: [Publicacion] -> Usuario -> [Publicacion]
filtrarPublicaciones [] _ = []
filtrarPublicaciones (p:ps) u 
    | esPublicacionDe u p = p : filtrarPublicaciones ps u
    | otherwise = filtrarPublicaciones ps u

-- Llama a la funcion filtrarPublicaciones pasandole como parametros las 
-- publicaciones de la red y el usuario del que queremos sus publicaciones,
--y si son sus publicaciones (se chequea con esPublicacionDe) se agrega a la
--lista
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red us = filtrarPublicaciones (publicaciones (red)) us

-- Ejercicio 7

-- FUNCION AUXILIAR
leDioLike :: Publicacion -> Usuario -> Bool
leDioLike (a, b, l) us 
    | pertenece us l = True
    | otherwise = False

-- FUNCION AUXILIAR
filtrarPorLikes :: [Publicacion] -> Usuario -> [Publicacion]
filtrarPorLikes [] _ = []
filtrarPorLikes (p:ps) u 
    | leDioLike p u = p : filtrarPorLikes ps u
    | otherwise = filtrarPorLikes ps u

-- Igual que en el ejercicio anterior se pasan los parametros a una funcion 
-- filtrarPorLikes, que en este caso se seleccionan segun si le dio me gusta 
--el usuario dado y se agregan a la lista
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red us = filtrarPorLikes (publicaciones (red)) us

--Ejercicio 8

-- La funcion se fija si las publicaciones que le gustan al usuario 1 son las 
-- mismas que le gustan al usuario 2
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red us1 us2 
    | pubQueLeGustanUs1 == pubQueLeGustanUs2 = True
    | otherwise = False
    where
        pubQueLeGustanUs1 = publicacionesQueLeGustanA red us1
        pubQueLeGustanUs2 = publicacionesQueLeGustanA red us2

-- Ejercicio 9

-- FUNCION AUXILIAR
cuentaLikes :: RedSocial -> Usuario -> [Publicacion] -> Int
cuentaLikes _ u [p] 
    | leDioLike p u = 1
    | otherwise = 0
cuentaLikes red u (p:ps)
    | leDioLike p u = 1 + cuentaLikes red u ps
    | otherwise = cuentaLikes red u ps

-- FUNCION AUXILIAR
likesPersonales :: RedSocial -> Usuario -> Usuario -> Int
likesPersonales red us1 us2 = cuentaLikes red us2 (publicacionesDe red us1)

-- FUNCION AUXILIAR
-- Si el numero de likes personales de un u2 es igual a la cantidad de 
-- publicaciones de un u1 devuelve True revisa si u2 es seguidor fiel de u1
esSeguidorFiel :: RedSocial -> Usuario -> Usuario -> Bool
esSeguidorFiel red u1 u2
    | likesDeU2AU1 == cantDePublicacionesDeU1 = True
    | likesDeU2AU1 < cantDePublicacionesDeU1 = False
    where
        likesDeU2AU1 = likesPersonales red u1 u2
        cantDePublicacionesDeU1 = len(publicacionesDe red u1)
    
 -- FUNCION AUXILIAR 
haySeguidorFiel :: RedSocial -> Usuario -> [Usuario] -> Bool   
haySeguidorFiel _ _ [] = False
haySeguidorFiel red u1 [u2]
    | esU2SeguidorFiel = True
    | otherwise = False
    where 
        esU2SeguidorFiel = esSeguidorFiel red u1 u2
haySeguidorFiel red u1 (u2:us2)
    | not esU2SeguidorFiel = haySeguidorFiel red u1 us2
    | otherwise = True
    where 
        esU2SeguidorFiel = esSeguidorFiel red u1 u2

-- Devuelve True si hay uno o mas usuarios que hayan dado like a todas las 
-- publicaciones del usuario
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel (_,_,[]) us = False
tieneUnSeguidorFiel red us = 
    haySeguidorFiel red us (likesDePublicacion (head(publicacionesDe red us)))
    
-- Ejercicio 10

-- FUNCION AUXILIAR
estanRelacionados :: RedSocial -> Usuario -> Usuario -> Bool
estanRelacionados red u1 u2 
    | pertenece u1 (amigosDe red u2) = True
    | otherwise = False

-- FUNCION AUXILIAR
amigosDeAmigos :: [Usuario] -> [Usuario] -> [Usuario] -> [Usuario]
amigosDeAmigos [] _ _ = []
amigosDeAmigos (f:fs) yaProbados us 
    | pertenece f yaProbados = amigosDeAmigos fs yaProbados us 
    | otherwise = f : amigosDeAmigos fs yaProbados us    

-- FUNCION AUXILIAR
revisarAmigos :: RedSocial -> [Usuario] -> Usuario -> [Usuario] -> Bool
revisarAmigos _ [] _ _ = False
revisarAmigos red (u:us) us2 yaProbados 
    | estanRelacionados red u us2 = True
    | pertenece u yaProbados = revisarAmigos red us us2 yaProbados
    | otherwise = revisarAmigos red amigosEnCadena us2 (u : yaProbados)
    where 
        amigosEnCadena = (amigosDeAmigos (amigosDe red u) yaProbados us)                                       

--LLama a una funcion con una lista de posibles amigos de us2, empezando
--por los amigos de us1, y luego agregando los amigos de amigos, una vez
--que se prueban se agregan a yaProbados para no repetir, asi hasta recorrer
--todos los posibles amigos de us2 conectados con us1
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red us1 us2 
    | estanRelacionados red us1 us2 = True
    | otherwise = revisarAmigos red (amigosDe red us1) us2 [us1]
