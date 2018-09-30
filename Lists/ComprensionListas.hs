a = [1,2,3,4]
b = [2,4,5,6]

--c = [x | and x <- a, x <- b] --Por cada elemento de a, imprime los elementos de b.
c = [x | x <- a, (elem x b)] -- Intersección.

u = [(x,y) | x <- a, y <- b] --Producto cartesiano

m = [2 * x | x<- a]

prueba :: [Int] -> [String]
prueba [1] = ["uno"]
prueba [2] = ["dos"]
prueba [3] = ["tres"]
prueba (x:xs) = prueba[x] ++ prueba[head xs]
   -- | elemento (x:xs) 1 && elemento (x:xs) 3 == True = meses [1] ++ meses [3] ++ meses [4]
   -- | otherwise = ["kjsd"]   -- | otherwise = ["kjsd"]

elemento :: (Eq a) => [a] -> [a]
elemento [] n = False
elemento (x:xs) n
    | n == x = True
    | otherwise = elemento xs n


---------_Ejercicio------------
--z = [50..10]
--ejercicio = [(x/2) | x <- z ]

divp :: Int -> [Int]
divp x = [ z | z <- [1..x-1], mod (x) z == 0]

sumal ::(Num a) => [a] -> a
sumal [] = 0
sumal (x:xs) = sumal xs + x

perf :: Int -> Bool
perf x = sumal(divp x) == x

amix :: Int -> Int -> Bool
amix x y = (sumal(divp x) == y) && (sumal(divp y) == x)
