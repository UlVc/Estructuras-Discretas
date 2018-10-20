module Prop where

import Data.List

--DEFINICIONES
-- Tipo de dato para representar las expresiones de la lógica proposicional
data Prop = Verdadero
          | Falso
          | Var String
          | Neg Prop
          | Conj Prop Prop
          | Disy Prop Prop
          | Impl Prop Prop
          | Syss Prop Prop
          deriving (Eq,Ord)

-- Sinónimo para representar el estado
type Estado = [(String, Prop)]

--Instancia de Show para el tipo Prop, para que sea legible lo que se imprime en consola
-- NO DEFINE COMPORTAMIENTO
instance Show Prop where
 show Verdadero = "V"
 show Falso = "F"
 show (Var x) = x
 show (Neg p) = "¬ " ++ show p
 show (Conj p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
 show (Disy p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
 show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
 show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

--EJERCICIOS

--EJERCICIO 1
variables :: Prop -> [String]
variables x = case x of Var x -> [x]
                        Neg w -> variables w
                        Conj x y -> variables x ++ variables y
                        Disy x y -> variables x ++ variables y
                        Impl x y -> variables x ++ variables y
                        Syss x y -> variables x ++ variables y

--EJERCICIO 2
quitaRepetidosPar ::(Eq a) => [(a,b)] -> [(a,b)]
quitaRepetidosPar n = case n of [] -> []
                                x:xs -> x:(quitaRepetidosPar (filter (\w -> fst x /= fst w) xs))
