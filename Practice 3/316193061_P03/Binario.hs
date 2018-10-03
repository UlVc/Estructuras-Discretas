module Binario where

 --Exercise 1.1
 data Binario = BaseUno | Cero Binario | Uno Binario deriving (Eq)
 
 instance Show Binario where
  show BaseUno = "1"
  show (Cero b) = show b ++ "0"
  show (Uno b) = show b ++ "1"

 --Exercise 3.1
 data Binario2 = BaseCero deriving (Eq)
 
 instance Show Binario2 where
  show BaseCero = "0"  

--Exercise 1.2
-- |Receives an integer and returns his representation in Binario form.
 natToBin :: Integer -> Binario
 natToBin n
    | (div n 2 == 0) = BaseUno 
 natToBin n = case a of 0 -> Cero (natToBin b) 
                        1 -> Uno (natToBin b)
             where
                a = mod n 2
                b = div n 2
--Exercise 1.3
-- |Receives a Binario type and returns his representation in integer form.
 binToNat :: Binario -> Int
 binToNat n = case n of Uno n -> (binToNat n)*2 + 1
                        Cero n -> (binToNat n) *2
                        BaseUno -> 1

--Exercise 1.4
-- |Receives a Binario type and returns the successor of this one in Binario form. 
 sucesor :: Binario -> Binario
 sucesor x = case x of Cero n -> Uno n
                       Uno n -> case n of Uno b -> Cero (sucesor n)
                                          BaseUno -> Cero (Cero n)
                                          Cero b -> Cero (Uno b)

--Exercise 1.5
{-|
  Receives a Binario type and returns the number of the bits that are 1.
  e.g.
  >>>  bitsEnecendidos (Uno (Cero (Uno (Uno (Cero BaseUno)))))
 4
-}
 bitsEncendidos :: Binario -> Int
 bitsEncendidos n = case n of Uno n -> (bitsEncendidos n) + 1
                              Cero n -> (bitsEncendidos n)
                              BaseUno -> 1