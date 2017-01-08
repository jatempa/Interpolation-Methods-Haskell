-- Calcular pendiente
pendiente :: Fractional b => [(b, b)] -> b
pendiente xs =
	let y0 = snd (xs !! 0)
	    y1 = snd (xs !! 1)
	    x0 = fst (xs !! 0)
	    x1 = fst (xs !! 1)
	in  (/) (y1 - y0) (x1 - x0)
-- Interpolacion Lineal
lineal :: Fractional b => [(b, b)] -> b -> b
lineal xs x =
	let x0 = snd (xs !! 0)
	    x1 = fst (xs !! 0)
	in  (+) x0 (pendiente xs * (x - x1))
-- Interpolacion Cuadratica
cuadratica :: Fractional b => [(b, b)] -> b -> b
cuadratica xs x =
	let b0 = snd (xs !! 0)
	    b1 = pendiente (init xs)
	    b2 = ((/) (pendiente (tail xs) - pendiente (init xs)) (fst (xs !! 2) - fst (xs !! 0)))
	in  b0 + (b1 * (x - (fst (xs !! 0)))) + (b2 * (x - (fst (xs !! 0))) * (x - (fst (xs !! 1))))
-- Metodo de Lagrange
lagrange :: (Eq t, Fractional t) => [(t, t)] -> t -> t
lagrange [] _ = 0
lagrange xs x =
	let n  = (length xs) - 1
	    ys = toGroup xs
	    p  = [product' (xs!!a) (ys!!a) x | a<-[0..n]]
	in  sum p

toGroup :: Eq a => [a] -> [[a]]
toGroup lista = 
	let n = (length lista) - 1
	    cs = group n [ys | xs <- lista, ys <- lista, xs /= ys]
	in  cs

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative n"

product' :: Fractional a => (a, a) -> [(a, t)] -> a -> a
product' xx yy a =
	let i = fst xx
	    j = snd xx
	    k = [fst ms | ms <- yy]
	    numerador = product (map(\x -> a - x) k)
	    denominador = product (map(\x -> i - x) k)
	    res = (*) ((/) numerador denominador) j
	in  res
-- Metodo principal
interpolar :: (Eq a, Fractional a, Show a) => [(a, a)] -> a -> [Char]
interpolar xs x | length xs == 2 = "f(" ++ show x ++ ") = " ++ show (lineal xs x)
                | length xs == 3 = "f(" ++ show x ++ ") = " ++ show (cuadratica xs x)
                | length xs >= 4 = "f(" ++ show x ++ ") = " ++ show (lagrange xs x)
                | otherwise      = "Error: parametros invalidos para su evaluacion."