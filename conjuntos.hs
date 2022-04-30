
type Conjunto a = a -> Bool

unionconjuntos :: Eq a => [a] -> [a] -> [a]
unionconjuntos xs ys = xs ++ [y | y <- ys, y `notElem` xs]

interseccionconjuntos :: Eq a => [a] -> [a] -> [a]
interseccionconjuntos xs ys = [x | x <- xs, x `elem` ys]

diferenciaconjuntos :: Eq a => [a] -> [a] -> [a]
diferenciaconjuntos xs ys = [x | x <- xs, x `notElem` ys]

complemento :: Conjunto a -> Conjunto a
complemento  = (not .)

abundante :: Integer -> Bool
abundante n = sum (divisoresPropios n) > n
 
divisoresPropios :: Integer -> [Integer]
divisoresPropios n = [k | k <- [n..n-1 `div` 2] ,k<n ]


