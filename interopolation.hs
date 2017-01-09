-- Calcular pendiente
pendiente :: Fractional b => [(b, b)] -> b
pendiente lst = 
    let y0 = snd (lst !! 0)
        y1 = snd (lst !! 1)
        x0 = fst (lst !! 0)
        x1 = fst (lst !! 1)
    in  (/) (y1 - y0) (x1 - x0)
-- Interpolacion Lineal
lineal :: Fractional b => [(b, b)] -> b -> b
lineal lst x =
    let x0 = snd (lst !! 0)
        x1 = fst (lst !! 0)
    in  (+) x0 (pendiente lst * (x - x1))
-- Interpolacion Cuadratica
cuadratica :: Fractional b => [(b, b)] -> b -> b
cuadratica lst x =
    let b0 = snd (lst !! 0)
        b1 = pendiente (init lst)
        b2 = ((/) (pendiente (tail lst) - pendiente (init lst)) (fst (lst !! 2) - fst (lst !! 0)))
    in  b0 + (b1 * (x - (fst (lst !! 0)))) + (b2 * (x - (fst (lst !! 0))) * (x - (fst (lst !! 1))))
-- Metodo de Lagrange
lagrange :: Fractional b => [(b, b)] -> b -> b
lagrange lst x =
    let n   = length lst - 1
        xs  = map fst lst
        ys  = map snd lst
        p i = product[(x - xs !! j) / (xs !! i - xs !! j) | j <- [0 .. n], i /= j]
        q   = [(ys !! i) * (p i)| i <- [0..n]]
    in  sum q
-- Metodo principal
interpolar :: (Fractional a, Show a) => [(a, a)] -> a -> [Char]
interpolar lst x 
    | length lst == 2 = "f(" ++ show x ++ ") = " ++ show (lineal lst x)
    | length lst == 3 = "f(" ++ show x ++ ") = " ++ show (cuadratica lst x)
    | length lst >= 4 = "f(" ++ show x ++ ") = " ++ show (lagrange lst x)
    | otherwise      = "Error: parametros invalidos para su evaluacion."