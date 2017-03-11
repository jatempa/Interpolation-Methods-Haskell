import Interpolacion

-- Metodo interpolar
interpolar :: (Fractional a, Show a) => [(a, a)] -> a -> [Char]
interpolar lst x
    | length lst == 2 = "f(" ++ show x ++ ") = " ++ show (lineal lst x)
    | length lst == 3 = "f(" ++ show x ++ ") = " ++ show (cuadratica lst x)
    | length lst >= 4 = "f(" ++ show x ++ ") = " ++ show (lagrange lst x)
    | otherwise      = "Error: parametros invalidos para su evaluacion."

main = do
    putStrLn "Introduce una lista de pares ordenados [(x1,y1),(x2,y3),...,(xN,yN)]"
    l <- getLine
    putStrLn "Introduce una incognita x"
    a <- getLine
    putStrLn $ "Resultado de " ++ interpolar (read l) (read a)
