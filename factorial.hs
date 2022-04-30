import System.IO
main = do
           putStr "Ingresa Factorial "
           hFlush stdout
           num <- getLine
           print (factorial (read num::Integer))

factorial::Integer->Integer
factorial n = if n == 0
              then 1
              else n*factorial(n-1)


