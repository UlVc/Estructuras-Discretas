module Relacion where

  import Data.List

  -- Sinónimo para representar relaciones binarias.
  type RelacionB a b = [(a,b)]

  -- OPERACIONES ENTRE RELACIONES

  --Ejercicio 1.1
  -- | Función que calcula la unión de dos relaciones binarias.
  unionR :: Eq a => Eq b => RelacionB a b -> RelacionB a b -> RelacionB a b
  unionR x y = nub $ x ++ y

  --Ejercicio 1.2
  -- | Función que calcula la intersección de dos relaciones binarias.
  interseccion :: Eq a => Eq b => RelacionB a b -> RelacionB a b -> RelacionB a b
  interseccion x y = x `intersect` y

  --Ejercicio 1.3
  -- | Función que calcula la diferencia de dos relaciones binarias.
  diferencia :: Eq a => Eq b => RelacionB a b -> RelacionB a b -> RelacionB a b
  diferencia r l = nub $ [c | c <- r, z <- l, c `notElem` l]

  --Ejercicio 1.4
  -- | Función que calcula el producto cartesiano dado dos conjuntos (dos listas).
  productoCartesiano :: [a] -> [b] -> RelacionB a b
  productoCartesiano xs ys = [(x,y) | x <- xs, y <- ys]

  -- PROPIEDADES

  -- | Función que dice si una relación tiene elementos que son reflexivos o no.
  refl :: Eq a => RelacionB a a -> Bool
  refl r
   | rs == [] = False
   | otherwise = and rs
    where
      rs = [elem(b,a)r | (a,b) <- r, a==b]

  --Ejercicio 2.1
  -- | Función que dice si una relación binaria es simétrica o no.
  simetrica :: Eq a => RelacionB a a -> Bool
  simetrica x = and [elem(b,a)x | (a,b) <- x]

  --Ejercicio 2.2
  -- | Función que dice si una relación binaria es antsimétrica o no.
  antisimetrica :: Eq a => RelacionB a a -> Bool
  antisimetrica r = and $ map not [elem(a,b)r | (b,a) <- r, b/=a]

  --Ejercicio 2.3
  -- | Función que dice si una relación binaria es reflexiva o no.
  reflexiva :: Eq a => [a] -> RelacionB a a -> Bool
  reflexiva l r = and [elem(a,a)r | a <- l]

  --Ejercicio 2.4
  -- | Función que dice si una relación binaria es antireflexiva o no.
  antireflexiva :: Eq a => RelacionB a a -> Bool
  antireflexiva x = not $ refl x 

  --Ejercicio 2.5
  -- | Función que dice si una relación binaria es asimétrica o no.
  asimetrica :: Eq a => RelacionB a a -> Bool
  asimetrica x 
   | (refl x) = False
   | otherwise = not $ simetrica x

  -- OPERACIONES SOBRE RELACIONES

  --Ejercicio 3.1
  -- | Función que calcula la inversa de una relación binaria.
  inversa :: Eq a => RelacionB a a -> RelacionB a a
  inversa [] = []
  inversa ((a,b):ys) = [(b,a)] ++ inversa ys

  --Ejercicio 3.2
  -- | Función que calcula el complemento de una relación binaria sobre el conjunto que recibe (como lista).
  complemento :: Eq a => [a] -> RelacionB a a -> RelacionB a a
  complemento c r = diferencia (productoCartesiano c c) r 
