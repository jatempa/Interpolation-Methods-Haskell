lineal :: Fractional b => [(b, b)] -> b -> b
lineal xs x = 
	let x0 = snd (xs !! 0)
	    x1 = fst (xs !! 0)
	in (+) x0 (pendiente xs * (x - x1))

cuadratica :: Fractional b => [(b, b)] -> b -> b
cuadratica xs x = 
	let b0 = snd (xs !! 0)
	    b1 = pendiente (init xs)
	    b2 = ((/) (pendiente (tail xs) - pendiente (init xs)) (fst (xs !! 2) - fst (xs !! 0)))
	in b0 + (b1 * (x - (fst (xs !! 0)))) + (b2 * (x - (fst (xs !! 0))) * (x - (fst (xs !! 1))))

pendiente :: Fractional b => [(b, b)] -> b
pendiente xs = 
	let y0 = snd (xs !! 0)
	    y1 = snd (xs !! 1)
	    x0 = fst (xs !! 0)
	    x1 = fst (xs !! 1)
	in (/) (y1 - y0) (x1 - x0)

interpolar :: (Fractional a, Show a) => [(a, a)] -> a -> [Char]
interpolar xs x | length xs == 2 = "f(x) = " ++ show (lineal xs x)
                | length xs == 3 = "f(x) = " ++ show (cuadratica xs x)
                | otherwise	     = "Error: parametros invalidos para su evaluacion."