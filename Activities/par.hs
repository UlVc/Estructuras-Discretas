par :: Int -> String
par n = if (mod n 2) == 0 then
	"Si es par" else "No es par"

pitagoras :: Float -> Float -> Float
pitagoras x y =  sqrt ((x ** 2) + (y ** 2)) 

incrementa :: Int -> Int
incrementa n = n + 1 

decrementa :: Int -> Int
decrementa n = n - 1

sumar :: String -> (Int,Int) -> Int
sumar "suma" x y = x + y
sumar "resta" x y = x - y