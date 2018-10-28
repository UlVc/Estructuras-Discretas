module Snoc where

-- Algebraic data type to build Snoc lists.

 data SnocList a = Mt | Snoc (SnocList a) a deriving (Show,Eq,Ord)

 -- Exercise 1.1:

{-|
  Function that receives a Snoc list and an element, and adds the element as the last item in the list.
  e.g.
  >>> addSnoc (Snoc (Snoc Mt 1) 2) 3
  Snoc (Snoc (Snoc (Mt 1) 2) 3
-}
 addSnoc :: SnocList a -> a -> SnocList a
 addSnoc n m = case n of Snoc x y -> Snoc (Snoc x y) m

 -- Exercise 1.2:

{-|
  Function that returns the last element of a Snoc list.
  e.g.
  >>> ultimo (Snoc (Snoc (Snoc Mt 1) 2) 3)
  3
-}
 ultimo :: SnocList a -> a
 ultimo n = case n of Mt -> error "No se permiten listas vacias."
                      Snoc x y -> y   

 -- Ecercise 1.3:

{-|
  Function that returns all the elements except the last element of a Snoc list.
  e.g.
  >>> resto (Snoc (Snoc (Snoc Mt 1) 2) 3)
  Snoc (Snoc Mt 1) 2
-}
 resto :: SnocList a -> SnocList a
 resto n = case n of Mt -> error "No se permiten listas vacias."
                     Snoc x y -> x
 
 -- Exercise 1.4:

{-|
  Function that returns the first element of a Snoc list.
  e.g.
  >>> cabeza (Snoc (Snoc (Snoc Mt 1) 2) 3)
  1
-}
 cabeza :: SnocList a -> a
 cabeza n = case n of Mt -> error "No se permiten listas vacias."
                      Snoc Mt x -> x
                      Snoc x y -> cabeza x

 -- Exercise 1.5:

{-|
  Function that returns all the elements except the first element of a Snoc list.
  e.g.
  >>> cola (Snoc (Snoc (Snoc Mt 1) 2) 3)
  Snoc (Snoc Mt 2) 3
-}
 cola :: SnocList a -> SnocList a
 cola n = case n of Mt -> error "No se permiten listas vacias."
                    Snoc Mt y -> Mt
                    Snoc x y -> Snoc (cola x) y

 -- Exercise 1.6:

{-|
  Function that returns the number of items in a Snoc list.
  e.g.
  >>> longitud (Snoc (Snoc (Snoc Mt 1) 2) 3)
  3
-}
 longitud :: SnocList a -> Int
 longitud n = case n of Mt -> 0
                        Snoc x y -> 1 + longitud x

 -- Extras:

 --Exercise 3.1:

{-|
  Function that receives a function, a Snoc List and applies the function to each element of the snoc list.
  e.g.
  >>> mapSnoc succ (Snoc (Snoc (Snoc Mt 1) 2) 3)
  Snoc (Snoc (Snoc (Mt 2) 3) 4
-}
 mapSnoc :: (a -> b) -> SnocList a -> SnocList b
 mapSnoc f n = case n of Snoc Mt y -> Snoc Mt $ f y
                         Snoc n m -> Snoc (mapSnoc f n) $ f m

{-|
  Function that receives a Snoc list and returns a list with the elements of the Snoc list.
  e.g.
  >>> list (Snoc (Snoc (Snoc Mt 1) 2) 3)
  [1,2,3]
-}
 list :: SnocList a -> [a]
 list n = case n of Snoc Mt y -> [y]
                    Snoc x y -> list x ++ [y]