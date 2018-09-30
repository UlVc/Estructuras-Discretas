suma :: Int -> Int -> Int
suma 0 m = m
suma n m = suma (pred n) (succ m)

par :: Int -> Bool
par 0 = True
par 1 = False
par n = par (n-2)

impar :: Int -> Bool 
impar 0 = False
impar 1 = True
impar n = impar (n-2)

par2 :: Int -> Bool
par2 0 = True
par2 n = impar2 (pred n)

potencia :: Int -> Int -> Int
potencia x 0 = 1
potencia x 1 = x
--potencia x y = m==x & potencia (x * x) (pred y)
	
impar2 :: Int -> Bool
impar2 0 = False
--impar2 n = par2(pred n)

car :: [a] -> a -- a (convención) es una variable de tipo, por lo cual puede tener cualquier tipo de variable. Si queremos que sea de enteros: [Int] -> Int
--car [e]  elemento -- e de elemento (puede ser cualquier cosa.)
car (x:xs) = x -- x cabeza, xs cola. Parenteisis = un solo elemento.
car [] = error "Lista no válida."

cdr :: [a] -> [a] -- la cola es una lista, la cabeza es un elemento!
cdr [e] = []
cdr (x:xs) = xs
cdr [] = error "No válido."

ultimo :: [a] -> a
ultimo [e] = e
ultimo (x:xs) = ultimo xs
ultimo [] = error "No válido."

toma :: Int -> [a] -> [a]
toma _ [] = [] -- No me importa cual es el primer parametro (_)
toma 0 _ = []
toma n (x:xs) = x:(toma(pred n) xs)

deja :: Int -> [a] -> [a]
deja 0 l = l
deja n [] = []
deja n (x:xs) = deja(pred n)xs

conc :: [a] -> [a] -> [a]
conc [] m = m
conc (x:xs) m = x:(conc xs m)

merge :: [a] -> [a] -> [a]
--merge [] []  []
merge [] m = m
merge m [] = m
merge (x:xs) (y:ys) = x:y:(merge xs ys)
