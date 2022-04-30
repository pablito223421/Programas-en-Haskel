--Main> a = "1/2+2/3 8/5-7/9 15/8÷2/5 3/5*9/5"
--Main> partir a ' '
--["1/2+2/3","8/5-7/9","15/8\247\&2/5","3/5*9/5"]
--Main>
--a = "1/2+2/3 8/5-7/9 15/8÷2/5 3/5*9/5"

import Data.Char

partir :: String ->Char-> [String]
partir [] c = [""]
partir (x:xs) c
        | x == c = "" : partir xs c
        | otherwise = (x : head (partir xs c)) :( tail (partir xs c))

quitar1 :: String ->Char-> [String]
quitar1 [] c = ["/"]
quitar1 (x:xs) c
        | x == c = "/" : quitar1 xs c
        | otherwise = (x : head (quitar1 xs c)) :( tail (quitar1 xs c))

--Main> a = "1/2+2/3 8/5-7/9 15/8÷2/5 3/5*9/5"
--Main> partir a ' '
--["1/2+2/3","8/5-7/9","15/8\247\&2/5","3/5*9/5"]
--Main> partir a '/'
--["1","2+2","3 8","5-7","9 15","8\247\&2","5 3","5*9","5"]

--Main> head $ partir a '/'
--"1"
--Main> head $ partir a ' '
--"1/2+2/3"
--Main> bloque=head $ partir a ' '
--Main> bloque
--"1/2+2/3"
--Main> partir bloque '+'
--["1/2","2/3"]
--Main> frac=partir bloque '+'
--Main> head frac  //primer/cabeza elemento de la lista
--"1/2"
--Main> last frac //segundo elemento de la lista
--"2/3"
--Main> partir (head frac) '/'
--["1","2"]
--Main> partir (last frac) '/'
--["2","3"]


