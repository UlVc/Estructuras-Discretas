data AE = Var String | Const Int | Op Operacion AE AE 
data Operacion = Suma | Resta | Mult | Div

eval :: AE -> Int
eval (Const x) = x
eval (Op x (Const y) (Const z)) = case x of Suma -> y + z 
                                            Resta -> y - z
                                            Mult -> y * z
                                            Div -> div y z

posNeg :: [Int] -> [String]
posNeg l = map((\ x -> (if (x < 0) then "negativo" else "positivo")))l

--Código viejo:

-- eval (Op Suma (Const x) (Const y)) = x + y
-- eval (Op Resta (Const x) (Const y)) = x - y
-- eval (Op Mult (Const x) (Const y)) = x * y
-- eval (Op Div (Const x) (Const y)) = div x y