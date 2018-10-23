module Prop where 
 
 import Data.List

--Definitions 

-- Data type to represent the expressions of the propositional logic.
 data Prop = Verdadero
           | Falso 
           | Var String
           | Neg Prop
           | Conj Prop Prop
           | Disy Prop Prop
           | Impl Prop Prop
           | Syss Prop Prop
           deriving (Eq,Ord)

-- Synonym to represent the state.
 type Estado = [(String, Prop)]

-- Instance of Show for the type Prop
 instance Show Prop where 
  show Verdadero = "V" -- V
  show Falso = "F" -- F
  show (Var x) = x -- P 
  show (Neg p) = "¬ " ++ show p -- ¬ P
  show (Conj p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")" -- (P ∧ Q)
  show (Disy p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")" -- (P ∨ Q)
  show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")" -- (P → Q)
  show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")" -- (P ↔ Q)

-- LOGICAL EQUIVALENCES

 -- Exercise 1.1

 -- |Function that use the rule of equivalence of elimination of operators to remove the implications and equivalences of propositions.
 eliminacion :: Prop -> Prop
 eliminacion x = case x of Verdadero -> x
                           Falso -> x
                           Var p -> Var p
                           Neg p -> Neg $ eliminacion p
                           Impl p q -> Disy (Neg $ eliminacion p) $ eliminacion q
                           Syss p q -> Conj (Disy (Neg $ eliminacion p) $ eliminacion q) (Disy (Neg $ eliminacion q) $ eliminacion p)
                           Conj p q -> Conj (eliminacion p) $ eliminacion q
                           Disy p q -> Disy (eliminacion p) $ eliminacion q

 -- Exercise 1.2

 -- |Function that uses De Morgan's equivalence rules to return equivalent propositions.
 deMorgan :: Prop -> Prop
 deMorgan n = case n of Var x -> Var x
                        Verdadero -> n
                        Falso -> n
                        Impl x y -> Impl (deMorgan x) $ deMorgan y
                        Syss x y -> Syss (deMorgan x) $ deMorgan y
                        Conj x y -> Conj (deMorgan x) $ deMorgan y
                        Disy x y -> Disy (deMorgan x) $ deMorgan y
                        Neg x -> case x of Var p -> Neg $ Var p
                                           Conj p q -> Disy (Neg $ deMorgan p) $ Neg $ deMorgan q
                                           Disy p q -> Conj (Neg $ deMorgan p) $ Neg $ deMorgan q
                                           Neg n -> deMorgan n
                                           otherwise -> Neg x

-- EVALUATION AND SYNTATIC ANALYSIS OF EXPRESSIONS

 -- |Function that builds a list with all the variable names that appear in a proposition.
 variables :: Prop -> [String]
 variables (Var x) = [x]
 variables (Neg p) = variables p
 variables (Conj p q) =  variables p `union` variables q
 variables (Disy p q) =  variables p `union` variables q
 variables (Impl p q) =  variables p `union` variables q
 variables (Syss p q) =  variables p `union` variables q
 variables p = []

 -- |Function that removes tuples that share the first element, leaving only the first appearance of this.
 quitaRepetidosPar ::(Eq a) => [(a,b)] -> [(a,b)]
 quitaRepetidosPar [] = []
 quitaRepetidosPar (x:xs) = x:(quitaRepetidosPar $ filter (\y -> fst x /= fst y) xs)

 -- |Function that calculates all the possible states of a proposition.
 estados :: Prop -> [Estado]
 estados p = nub -- We eliminate states that may be repeated.
            -- We order each of the states.
            $ map sort
              --  We leave a single appearance of the variables in each state.
              $ map quitaRepetidosPar
              -- We calculate all the permutations of the combinations of the Variables with the logical constants.
              $ permutations [(x,y) | x <- (variables p), y <- [Verdadero,Falso]]

-- |Function that looks for the string in the state and returns True if it was found or False if it was not.
 busca :: String -> Estado -> Bool
 busca s e = case (head $ map (\x -> snd x) $ filter (\x -> (s == fst x)) e) of Verdadero -> True; otherwise -> False

 {-|
  Function that receives a list of logical propositions (hypothesis) and returns a proposition with the disjunction of each hypothesis.
  e.g.
  >>>  toConj [(Disy (Var "P") (Var "Q")),(Impl (Var "P") Verdadero), (Var "U")]
 ((P ∨ Q) ∧ ((P → V) ∧ (U ∧ V))) 
-}
 toConj :: [Prop] -> Prop
 toConj x = case x of [] -> Verdadero
                      x:xs -> Conj x $ toConj xs

 -- Exercise 2.1

 -- |Function that receives a proposition and a state and interprets the proposition under the state.
 interp :: Prop -> Estado -> Bool
 interp p e = case a of Verdadero -> True
                        Falso -> False
                        Var x -> busca x e
                        Neg x -> not $ interp x e
                        Disy p q -> interp p e || interp q e
                        Conj p q -> interp p e && interp q e
                        Impl p q -> not $ interp p e || interp q e
                        Syss p q -> (not $ interp p e || interp q e) && (not $ interp q e || interp p e)
        where
           a = deMorgan $ eliminacion p

 -- Exercise 2.2

  {-|
  Function that receives a logical proposition and tells us if it is Tautology, Contradiction or Contingency.
  e.g.
  >>>  truthTable (Impl (Conj (Impl (Var "P") (Var "Q")) (Var "P")) (Var "Q"))
 "Tautología"
-}
 truthTable :: Prop -> String
 truthTable x
  | length a == b = "Tautología"
  | b == 0 = "Contradicción"
  | otherwise = "Contingencia"
            where
              a = map (interp x) $ estados x
              b = length $ filter (\x -> (case x of True -> True; otherwise -> False)) a

 -- Exercise 2.3 
 
 -- |Function that says if a logical argument is valid or not.
 correcto :: [Prop] -> Prop -> Bool 
 correcto n q = case (truthTable $ Impl (toConj n) q) of "Tautología" -> True; otherwise -> False
