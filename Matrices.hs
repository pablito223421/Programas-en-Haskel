import Data.Array

type Matriz a = Array (Int,Int) a

-- let x = matriznumeros[[2,3,5,6],[4,6,2,1]]
-- let y = matriznumeros[[4,2,1,11],[22,41,4,9]]
--listanumeros(sumaMatriz x y)
sumaMatriz:: Num a => Matriz a -> Matriz a -> Matriz a
sumaMatriz p q = 
    array ((1,1),(m,n)) [((i,j),p!(i,j)+q!(i,j)) | 
                         i <- [1..m], j <- [1..n]]
    where (m,n) = dimensionmatriz p

--dimensionmatriz(matriznumeros[[2,3,5,6],[4,6,2,1]])--
dimensionmatriz :: Num a => Matriz a -> (Int,Int)
dimensionmatriz p = (numeroFilas p, numeroColumnas p)

listanumeros :: Num a => Matriz a -> [[a]]
listanumeros p = separa (numeroColumnas p) (elems p)


numeroColumnas:: Num a => Matriz a -> Int
numeroColumnas = snd . snd . bounds

numeroFilas :: Num a => Matriz a -> Int
numeroFilas = fst . snd . bounds

--matriznumeros[[2,3,5,6],[4,6,2,1]]--
matriznumeros :: Num a => [[a]] -> Matriz a
matriznumeros xss = listArray ((1,1),(m,n)) (concat xss)
                 where m = length xss
                       n = length (head xss)

separa :: Int -> [a] -> [[a]]
separa _ [] = []
separa n xs = take n xs : separa n (drop n xs)