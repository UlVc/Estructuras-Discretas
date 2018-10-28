module Nat where

  data Nat = Cero | S Nat deriving (Eq, Ord)

  instance Show Nat where
    show Cero = "0"
    show (S n) = "S (" ++ show n ++ ")"


  suma :: Nat -> Nat -> Nat
  suma n Cero = n
  suma n (S m) = S(suma(n) m)

  multiplica :: Nat -> Nat -> Nat
  multiplica n Cero = Cero
  multiplica n (S m) = (suma n (multiplica n m))  


  spar :: Nat -> Nat
  spar n = multiplica n (S n) 

  simpar :: Nat -> Nat
  simpar n = multiplica (S n) (S n)
