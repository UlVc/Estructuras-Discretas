data AE = Var String | Const Int | Op Operacion AE AE deriving (Show)

data Operacion = Suma | Resta | Mult | Div deriving (Show)

type Estado = [(String,AE)]

opera :: Operacion -> (Int -> Int -> Int)
opera n = case n of Suma -> (\x y -> (x + y)); Resta -> (\x y -> (x - y)); Mult -> (\x y -> (x * y)); Div -> (\x y -> (div x y))

busca :: String -> Estado -> AE
busca s e = head (map (\x -> snd(x))(filter (\x -> (s == fst (x))) e))

eval :: AE -> Estado -> Int
eval (Const x) _ = x
eval (Op x (Const y) (Const z)) _ = ((opera x) y z)
eval (Op x (Var s) (Var p)) e = (eval (Op x (busca s e) (busca p e)) [])
eval (Op x (Var s) (Const c)) e = (eval (Op x (busca s e) (Const c)) [])
eval (Op x (Const c) (Var p)) e = (eval (Op x (Const c) (busca p e)) [])