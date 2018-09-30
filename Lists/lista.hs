a = [1,2,3,4]
b = [2,4,5,6]

--c = [x | and x <- a, x <- b] --Por cada elemento de a, imprime los elementos de b.
c = [x | x <- a, (elem x b)] -- Intersección.

u = [(x,y) | x <- a, y <- b] --Producto cartesiano

m = [2 * x | x<- a]

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
meses [] = error "Inserte un número del 1 al 12."
meses [z] = [mes z]
meses (x:xs) = [mes x] ++ meses xs