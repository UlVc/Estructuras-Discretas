
--Función que compara dos Int, regresa :
-- "EQ" si son iguales
-- "GT" si el primero es mayor al segundo
-- "LT" si el primero es menor al segundo
compara :: Int -> Int -> String
compara n m 
 | n > m = "GT"
 | n < m = "LT"
 | n == m = "EQ"

--Versión con otherwise
compara2 :: Int -> Int -> String
compara2 n m 
 | n > m = "GT"
 | n < m = "LT"
 | otherwise = "EQ"

--Función que regresa las iniciales de un nombre
iniciales :: String -> String -> String
iniciales nombre apellido = [n] ++ ". " ++ [a] ++ "."
 where (n:_) = nombre
       (a:_) = apellido

--Función que dice si una cadena es palindroma o no
palindromo :: String -> Bool
palindromo str = let rts = reverse str in str == rts 

--Definición del tipo de Dato Figura 
data Figura = Circulo Float | Cuadrado Float | Rectangulo Float Float | Triangulo Float Float  deriving (Show,Eq)

--Funcion que calcula el área de una Figura
area :: Figura -> Float
area (Circulo radio) = pi * (radio ** 2)
area (Cuadrado lado) = lado ** 2
area (Rectangulo base altura) = base * altura
area (Triangulo base altura) = (base * altura) / 2

--Función que suma uno a cada elemento de una lista de enteros
suma1 :: [Int] -> [Int]
suma1 l = map suc l 

--Función que calcula el área de un Triángulo usando la fórmula de Heron.
areaHeron :: Float -> Float -> Float -> Float
areaHeron a b c = sqrt (s * (s - a) * (s - b) * (s - c)) 
 where s = (a + b + c) / 2

--Loki sin if 
loki :: Int -> Bool -> String
loki t v
 | ((t < 25) && (t > 15) && (not v)) || ((t < 30) && (t > 20) && v) = "Sale jugar"
 | otherwise = "No sale a jugar"

data Lista a = Empty | Cons a (Lista a) deriving (Show, Eq) 
