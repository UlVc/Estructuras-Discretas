--Villavicencio Cárdenas Ulrich.
--Borrás Pablo Vianey Aileen 316033619

data Figura = Circulo Float | Cuadrado Float | Rectangulo Float Float | Triangulo Float Float deriving (Show,Eq)

area :: Figura -> Float
area (Circulo r) = (r**2)*pi
area (Cuadrado l) = l*l
area (Rectangulo b h) = b * h
area (Triangulo b l) = (b * l)/2 

loki :: Int -> Bool -> String
loki t False
    | ( t < 25) && (t > 15) = "Sale a jugar."
    | otherwise = "No sale a jugar."
loki t True 
    | (t < 30) && ( t > 20) = "Sale a jugar"
    | otherwise = "No sale a jugar "

suma1 :: [Int] -> [Int]
suma1 (x:xs) = map (1+) (x:xs)

areaHeron :: Float -> Float -> Float -> Float
areaHeron a b c = sqrt(s*(s-a)*(s-b)*(s-c)) 
        where s = (a + b + c)/2 