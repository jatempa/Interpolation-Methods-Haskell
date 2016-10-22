lineal :: Fractional b => [(b, b)] -> b -> b
lineal xs x = (+) (_x0 xs) (pendiente xs * (x - (_x1 xs)))

cuadratica :: Fractional b => [(b, b)] -> b -> b
cuadratica xs x = 
	let _b0 = _x0 xs
	    _b1 = pendiente (init xs)
	    _b2 = ((/) (pendiente (tail xs) - pendiente (init xs)) (fst (xs !! 2) - fst (xs !! 0)))
	in _b0 + (_b1 * (x - (_x1 xs))) + (_b2 * (x - (_x1 xs)) * (x - (_x2 xs)))

pendiente :: Fractional b => [(b, b)] -> b
pendiente xs = (/) (snd (xs !! 1) - snd (xs !! 0)) (fst (xs !! 1) - fst (xs !! 0))

_x0 :: [(a, b)] -> b
_x0 xs = snd (xs !! 0)

_x1 :: [(a, b)] -> a
_x1 xs = fst (xs !! 0)

_x2 :: [(a, b)] -> a
_x2 xs = fst (xs !! 1)

interpolar :: (Fractional a, Show a) => [(a, a)] -> a -> [Char]
interpolar xs x | length xs == 2 = "f(x) = " ++ show (lineal xs x)
                | length xs == 3 = "f(x) = " ++ show (cuadratica xs x)
                | otherwise	     = "Error: parametros invalidos para su evaluacion."