--ESTRUCTURAS DISCRETAS 2019-1
--Práctica 1

--Ejercicio 1.1:
areaCirculo :: Float -> Float
areaCirculo r = pi * (r ^ 2)

--Ejercicio 1.2:
distancia :: (Float , Float ) -> (Float , Float ) -> Float
distancia (x,y) (x2,y2) = sqrt (((x2 - x) ^ 2) + ((y2 - y) ^ 2))

--Ejercicio 1.3:
imp :: Bool -> Bool -> Bool
imp x y = (not x) || y 

--Ejercicio 1.4:
xor :: Bool -> Bool -> Bool
xor p q = not (p || not (q)) || (p && not (q))

--Ejercicio 1.5:
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


--Ejercicio 1.6:
calculadora :: String -> (Float,Float) -> Float
calculadora "sum" (x,y) = x + y
calculadora "rest" (x,y) = x - y
calculadora "first" (x,y) = x
calculadora "last" (x,y) = y
calculadora "div" (x,y) = x / y
calculadora "pow" (x,y) = x ** y
calculadora "mul" (x,y) = (x)*(y)

--Ejercicio 1.7:
loki :: Int -> Bool -> String
loki temp verano =  if verano == True
                    then
                        if temp > 20 && temp <30
                        then "Sale a jugar."
                        else "No sale a jugar."
                    else
                        if temp > 15 && temp <25
                        then "Sale a jugar."
                        else "No sale a jugar."
--loki temp verano if verano == True then (if ((temp >= 20) && (temp <= 30)) then "Sale a jugar." else "No sale a jugar.") else (if (15<temp<25) then "Sale a jugar." else "No sale a jugar.")

--Ejercicio 1.8:
monos :: Bool -> Bool -> String
monos mono1 mono2 = if (mono1 == mono2) then "Hay problemas." else "No hay problemas."

suma :: Int -> Int -> Int
suma 0 m = m
suma n m = suma (pred n) (succ m)

--Ejercicio 1.9:
multiplica :: Int -> Int -> Int
multiplica 0 m = 0
multiplica 1 m = m
multiplica n m = suma m (multiplica (pred n) m) --Pensar en n como un contador y en cómo hacer para que se siga manteniendo dos parametros por pasar (lo mismo con potencia).

--Ejercicio 1.10:
potencia :: Int -> Int -> Int
potencia x 0 = 1
potencia x 1 = x
potencia x y = multiplica x (potencia x (pred y)) --Pensar en y como un contador.

