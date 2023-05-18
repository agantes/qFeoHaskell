module TestsDeTP where

import Test.HUnit
import Solucion

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

-- Genera relaciones entre toda la red y un usuario dado
relacionesCon :: [Usuario] -> Usuario -> [Relacion]
relacionesCon [u] _ = []
relacionesCon (u:us) v = [(v,u)] ++ relacionesCon us v

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
    " Existe secuencia II " ~: (existeSecuenciaDeAmigos red13 (3,"C") (4,"D")) ~?= False,
    " No existe secuencia " ~: (existeSecuenciaDeAmigos red12 (2,"B") (1,"A")) ~?= False
    ]    

testSuite = TestList [
    testNombresDeUsuarios, testAmigosDe,testCantidadDeAmigos,
    testUsuarioConMasAmigos, testEstaRobertoCarlos, testPublicacionesDe,
    testPublicacionesQueLeGustanA, testLesGustanLasMismasPublicaciones,
    testTieneUnSeguidorFiel, testExisteSecuenciaDeAmigos
    ]

run = runTestTT testSuite