import Interpolacion
import Graphics.EasyPlot

-- Metodo interpolar
interpolar :: Fractional b => [(b, b)] -> b -> b
interpolar lst x
    | length lst == 2 = lineal lst x
    | length lst == 3 = cuadratica lst x
    | length lst >= 4 = lagrange lst x

--Ejemplo 
--[(0,0), (10,250), (15,350), (22,655), (25,890), (30,910)]
main = do
    putStrLn "Introduce una lista de pares ordenados [(x1,y1),(x2,y3),...,(xN,yN)]"
    l <- getLine
    putStrLn "Introduce una incognita x"
    x <- getLine
    let y = interpolar (read l) (read x)
    putStrLn $ "Resultado de f(" ++ x ++ ") = " ++ show y
    plot X11 [Data2D [Title "Datos", Color Red] [] (read l), Data2D [Title "Incognita", Style Points, Color Blue] [] [(read x,y)]]