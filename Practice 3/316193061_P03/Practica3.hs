module Practica3 where

 import Binario

--Exercise 2.1
-- |Receives a list of integers and returns a list of Binarios with the representation in binary of each of them.
 binarios :: [Integer] -> [Binario]
 binarios l = map natToBin l

--Exercise 2.2
{-|
  Receives a list of numbers in its Binario form and returns a new list with only those that are even.
  e.g.
  >>> pares (binarios [20..25])
  [10100,10110,11000]
-}
 pares :: [Binario] -> [Binario]
 pares x = filter (\ x -> (case x of Cero x -> True; otherwise -> False))x

--Exercise 2.3
-- |Receives a list of Strings and returns a new list with the Strings that their length are less than 8.
 tooLong :: [String] -> [String]
 tooLong x = filter (\ x -> length x <= 7) x

--Exercise 2.4
{-|
  Receives an integer n and return the first n+1 fibonacci sequence in a list form.
  e.g.
  >>> sFibonacci 3
  [0,1,1,2]
-}
 sFibonacci :: Int -> [Int]
 sFibonacci x = [fibonacci x | x <- [0..x]]

-- |Returns the position of the fibonacci number from the specified position.
 fibonacci :: Int -> Int
 fibonacci n = case n of 0 -> 0; 1 -> 1; otherwise -> fibonacci(n-1) + fibonacci(n-2)

--Exercise 2.5
-- |Receives a list of comparable items and an item of the list and return a new list without the item.
 quitaElemento :: (Eq a) => [a] -> a -> [a]
 quitaElemento x n = filter (\ x -> x /= n) x