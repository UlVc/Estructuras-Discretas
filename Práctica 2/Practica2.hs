--ESTRUCTURAS DISCRETAS 2019-1
--Práctica 2

module Practica2 where

--DEFINICIÓN DE LISTAS

--1.1: Naturales.
nat = [0..]
--1.2: Multiplos de diez.
multiplosDiez = [10*x | x <- nat]
--1.3: potencias de 2.
potenciasDos = [2^x | x <- nat]
--1.4: Números pares.
pares = [2*x | x <- nat]
--1.5: años desde el año de tu nacimiento.
anosVividos = [2000..2018]


--DEFINICIÓN DE FUNCIONES

--Concatena dos listas.
conc :: [a] -> [a] -> [a]
conc [] m = m
conc (x:xs) m = x:(conc xs m)

--Ejercicio 2.1:

--Devuelve la posición del número de fibonacci a partir de la posición especificada.
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)

--Ejercicio 2.2:

--Verifica si un elemento está en la lista, si está devuelve True y si no, devuelve un False.
elemento :: (Eq a) => [a] -> a -> Bool
elemento [] n = False
elemento (x:xs) n
    | n == x = True
    | otherwise = elemento xs n

--Ejercicio 2.3:

--Suma todos los elementos que tiene la lista.
sumaLista ::(Num a) => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = sumaLista xs + x --Suma los elementos de una lista.

--Ejercicio 2.4:

mes :: Int -> String
mes 1 = "Enero."
mes 2 = "Febrero."
mes 3 = "Marzo."
mes 4 = "Abril."
mes 5 = "Mayo."
mes 6 = "Junio."
mes 7 = "Julio."
mes 8 = "Agosto."
mes 9 = "Septiembre."
mes 10 = "Octubre."
mes 11 = "Noviembre."
mes 12 = "Diciembre."
mes x = "Numero no correcto, introducir un numero del 1 al 12."

meses :: [Int] -> [String]
--Devuelve el mes o meses a partir del número o números de los meses. Ej: [1,2] es igual a ["Enero","Febrero"]
meses [] = error "Inserte un número del 1 al 12."
meses [z] = [mes z]
meses (x:xs) = conc ([mes x]) (meses xs)

--Ejercicio 2.5:

--Devuelve los divores propios de un número dado.
divisoresPropios :: Int -> [Int]
divisoresPropios x = [ z | z <- [1..x-1], mod (x) z == 0]

--Ejercicio 2.6:

--Devuelve True si un número es perfecto y false si no lo es.
esPerfecto :: Int -> Bool
esPerfecto x = sumaLista(divisoresPropios x) == x

--Ejercicio 2.7:

--Devuelve True si dos números son amigos y false si no lo son.
sonAmigos :: Int -> Int -> Bool
sonAmigos x y = (sumaLista(divisoresPropios x) == y) && (sumaLista(divisoresPropios y) == x)

--Ejercicio 2.8:

--Devuelve la suma de cada elemento que tenga un número. Ej: 123 = 1 + 2 + 3
supersuma :: Int -> Int
supersuma x
    | x < 10 = x
    | otherwise = (mod x 10) + supersuma (div x 10) --Suma de las cifras de un número natural.

--Ejercicio 2.9:

--Devuelve cómo se escribe un número menor a 100 en japonés.
japones :: Int -> String
japones 0 = "rei"
japones 1 = "ichi"
japones 2 = "ni"
japones 3 = "san"
japones 4 = "yon"
japones 5 = "go"
japones 6 = "roku"
japones 7 = "nana"
japones 8 = "haci"
japones 9 = "kyu"
japones 10 = "ju"
japones n
    | mod n 10 == 0 = conc (japones (div n 10)) (" ju")
    | otherwise = conc (conc (japones (div n 10)) (" ju ")) (japones (mod n 10)) --japones (div n 10) ++ " ju " ++ japones (mod n 10)