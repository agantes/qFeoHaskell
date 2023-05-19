module TestsDeTP where

import Test.HUnit
import Solucion

-------------------------------------------------------------------------------------------------------
-- Redes catedra

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

-- Notar que el orden en el que aparecen los usuarios es indistinto
relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) 
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

-------------------------------------------------------------------------------------------------------

red1 = ([(1,"A"),(2,"B"),(3,"C"),(4,"D"),(5,"E")], [], [])

red2 = ([],[],[])

red3 = ([(1,"A")], [], [((1,"A"),"Hola",[]),((1,"A"),"Como Andas?",[]),((1,"A"),"qFeoHaskell",[])])

red4 = ([(1,"A"),(2,"B"),(3,"C"),(4,"D"),(5,"E")], [((2,"B"),(3,"C")), ((3,"C"),(4,"D"))], 
 [((1,"A"),"qFeoHaskell",[(3,"C")])])

red5 = ([(1,"A"),(2,"B"),(3,"C"),(4,"D"),(5,"E")],
 [((2,"B"),(3,"C")), ((3,"C"),(4,"D")), ((2,"B"),(4,"D"))],[((3,"C"),"Hola",[])]
 )

red6 = ([(1,"A"),(2,"B")],[],
 [((1,"A"),"Hola",[(2,"B")]),((1,"A"),"Como Andas?",[]),((1,"A"),"qFeoHaskell",[(2,"B")])]
 )

red7 = ([(1,"Juan"),(2,"Maria")],[],[
    ((2,"Maria"),"Hola",[(1,"Juan"),(2,"Maria")]),
    ((2,"Maria"),"Hoy llueve",[(1,"Juan"),(2,"Maria")])]) --les gustan las mismas publicaciones
    
red8 = ([(1,"Juan"),(2,"Maria")],[],[
    ((1,"Juan"),"Hola",[(2,"Maria")]),
    ((2,"Maria"),"Hoy llueve",[(1,"Juan")])]) --les gustan distintas publicaciones

red9 = ([(1,"A"),(2,"B"),(3,"C"),(4,"D"),(5,"E")], [], [((1,"A"),"PrimeraPubli",[(3,"C"),(2,"B")]),
                                                        ((1,"A"),"SegundaPubli",[(3,"C")]),
                                                        ((2,"B"),"Hola",[]),
                                                        ((1,"A"),"TerceraPubli",[(3,"C")])
                                                        ]
        )    --el usuario (3,"C") es seguidor fiel de (1,"A")

red10 = ([(1,"A"),(2,"B"),(3,"C"),(4,"D"),(5,"E"),(6,"A")], [], [])

red11 = (([(1,"A"),(2,"B"),(3,"C"),(4,"D"),(5,"E")], [((1,"A"),(4,"D")),((1,"A"),(5,"E")),((4,"D"),(3,"C")),((2,"B"),(3,"C"))], []))

red11bis = (([(1,"A"),(2,"B"),(3,"C"),(4,"D"),(5,"E"),(6,"F"),(7,"G")], 
            [((1,"A"),(4,"D")),((3,"C"),(5,"E")),((4,"D"),(3,"C")),((5,"E"),(6,"F")),
             ((7,"G"),(6,"F")),((2,"B"),(7,"G"))], []))

red12 = (([(1,"A"),(2,"B"),(3,"C"),(4,"D"),(5,"E"),(6,"F")], [((1,"A"),(4,"D")),((1,"A"),(5,"E")),((4,"D"),(3,"C")),((2,"B"),(6,"F"))], []))

red13 = (([(1,"A"),(2,"B"),(3,"C"),(4,"D")], [((1,"A"), (2,"B")), ((2,"B"), (3,"C")), ((3,"C"), (1,"A"))], []))

-- El test ideal deberia tener un millon como numDePrueba
numDePrueba = 10

usuariosHayUnRobertoCarlos = nUsuarios (numDePrueba+1) ++ [(12, "Roberto")]
relacionesHayUnRobertoCarlos = relacionesCon usuariosHayUnRobertoCarlos (12, "Roberto")
hayUnRobertoCarlos = (
    usuariosHayUnRobertoCarlos,
    relacionesHayUnRobertoCarlos,
    []
    )

usuariosNoHayUnRobertoCarlos = nUsuarios numDePrueba ++ [(11, "Roberto")]
relacionesNoHayUnRobertoCarlos = relacionesCon usuariosNoHayUnRobertoCarlos (11, "Roberto")
noHayUnRobertoCarlos = (
    usuariosNoHayUnRobertoCarlos,
    relacionesNoHayUnRobertoCarlos,
    []
    )

-------------------------------------------------------------------------------------------------------   
-- Funciones auxiliares de testeo

-- Genera una lista de n usuarios con diferente id pero mismo nombre
nUsuarios :: Integer -> [Usuario]
nUsuarios 0 = []
nUsuarios n = [(n,"A")] ++ nUsuarios (n-1)

-- Genera relaciones entre toda la red y un usuario dado concatenando relaciones
-- variando los usuarios de red a traves de recursividad
relacionesCon :: [Usuario] -> Usuario -> [Relacion]
relacionesCon [u] _ = []
relacionesCon (u:us) v = [(v,u)] ++ relacionesCon us v

-------------------------------------------------------------------------------------------------------
--Tests Catedra
testsCatedra = TestList [
    " nombresDeUsuarios 1" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],

    " amigosDe 1" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4],

    " cantidadDeAmigos 1" ~: (cantidadDeAmigos redA usuario1) ~?= 2,

    " usuarioConMasAmigos 1" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4],

    " estaRobertoCarlos 1" ~: (estaRobertoCarlos redA) ~?= False,

    " publicacionesDe 1" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],

    " publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1],

    " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True,

    " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True,

    " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True
 ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-------------------------------------------------------------------------------------------------------

-- Tests ejercicio 1 --
testNombresDeUsuarios = TestList [
    " Lista de nombres vacia " ~: (nombresDeUsuarios red2) ~?= [],
    " Lista con un elemento " ~: (nombresDeUsuarios red3) ~?= ["A"],
    " Lista con varios nombres " ~: (nombresDeUsuarios red1) ~?= ["A","B","C","D","E"],
    " Lista con varios nombres con repetidos " ~: (nombresDeUsuarios red10) ~?= ["B","C","D","E","A"]
    ]

-- Tests ejercicio 2 --
testAmigosDe = TestList [
    " Lista de Relaciones vacia " ~: (amigosDe red1 (1,"A")) ~?= [],
    " Sin amigos " ~: (amigosDe red4 (1,"A")) ~?= [],
    " Un Par de Amigos " ~: (amigosDe red4 (3,"C")) ~?= [(2,"B"),(4,"D")]
    ]

-- Tests ejercicio 3 -- 
testCantidadDeAmigos = TestList [
    " Cant de amigos = 0 " ~: (cantidadDeAmigos red3 (1,"A")) ~?= 0,
    " Cant de amigos = 1 " ~: (cantidadDeAmigos red4 (2,"B")) ~?= 1,
    " Cant de amigos > 1 " ~: (cantidadDeAmigos red4 (3,"C")) ~?= 2 
    ]     

-- Tests ejercicio 4 --
testUsuarioConMasAmigos = TestList [
    " Hay un usuario con mas amigos " ~: (usuarioConMasAmigos red4) ~?= (3,"C"),
    " Ninguno tiene amigos " ~: (usuarioConMasAmigos red1) ~?= (1,"A"),
    " Tienen + de 1 amigo, pero misma cantidad " ~: (usuarioConMasAmigos red5) ~?= (2,"B")
    ]

-- Tests ejercicio 5 --
testEstaRobertoCarlos = TestList [
    "Esta Roberto Carlos" ~: (usuarioConNYUnAmigos hayUnRobertoCarlos 10) ~?= True,
    "No esta Roberto Carlos" ~: (usuarioConNYUnAmigos noHayUnRobertoCarlos 10) ~?= False
    ]

-- Tests ejercicio 6 -- 
testPublicacionesDe = TestList [
    " Sin Publicaciones (lista vacia de publicaciones) " ~: (publicacionesDe red1 (1,"A")) ~?= [],
    " Sin Publicaciones " ~: (publicacionesDe red5 (1,"A")) ~?= [],
    " Con 1 publicacion " ~: (publicacionesDe red5 (3,"C")) ~?= [((3,"C"),"Hola",[])],
    " Mas de 1 publicacion " ~: (publicacionesDe red3 (1,"A")) ~?= 
        [((1,"A"),"Hola",[]),((1,"A"),"Como Andas?",[]),((1,"A"),"qFeoHaskell",[])]
    ]    

-- Tests ejercicio 7 --
testPublicacionesQueLeGustanA = TestList [
    " Le gustan 2 publicaciones " ~: (publicacionesQueLeGustanA red6 (2,"B")) ~?= 
        [((1,"A"),"Hola",[(2,"B")]),((1,"A"),"qFeoHaskell",[(2,"B")])],
    " No le dio me gusta a nada " ~: (publicacionesQueLeGustanA red6 (1,"A")) ~?= [],
    " Le gusta 1 publicacion " ~: (publicacionesQueLeGustanA red4 (3,"C")) ~?= [((1,"A"),"qFeoHaskell",[(3,"C")])]
    ]

-- Test ejercicio 8 --
testLesGustanLasMismasPublicaciones = TestList [
    " Le gustan las mismas publicaciones " ~: (lesGustanLasMismasPublicaciones red7 (1,"Juan") (2,"Maria")) ~?= True,
    " Les gustan diferentes publicaciones " ~: (lesGustanLasMismasPublicaciones red8 (1,"Juan") (2,"Maria")) ~?= False
    ]    

-- Test ejercicio 9 -- 
testTieneUnSeguidorFiel = TestList [
    " Tiene seguidor fiel " ~: (tieneUnSeguidorFiel red9 (1,"A")) ~?= True,
    " No tiene un seguidor fiel " ~: (tieneUnSeguidorFiel red9 (2,"B")) ~?= False,
    " No tiene publicaciones " ~: (tieneUnSeguidorFiel red1 (1,"A")) ~?= False
    ]

-- Test ejercicio 10 -- 
testExisteSecuenciaDeAmigos = TestList [
    " Amigos directos " ~: (existeSecuenciaDeAmigos red4 (2,"B") (3,"C")) ~?= True,
    " Existe secuencia " ~: (existeSecuenciaDeAmigos red11 (2,"B") (1,"A")) ~?= True,
    " Existe secuencia II " ~: (existeSecuenciaDeAmigos red11bis (2,"B") (1,"A")) ~?= True,
    " No existe secuencia (bucle) " ~: (existeSecuenciaDeAmigos red13 (3,"C") (4,"D")) ~?= False,
    " No existe secuencia " ~: (existeSecuenciaDeAmigos red12 (2,"B") (1,"A")) ~?= False
    ]    

testSuite = TestList [
    testNombresDeUsuarios, testAmigosDe,testCantidadDeAmigos,
    testUsuarioConMasAmigos, testEstaRobertoCarlos, testPublicacionesDe,
    testPublicacionesQueLeGustanA, testLesGustanLasMismasPublicaciones,
    testTieneUnSeguidorFiel, testExisteSecuenciaDeAmigos, testsCatedra
    ]

run = runTestTT testSuite