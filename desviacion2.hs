import Data.List
varianza :: Floating a => [a] -> a
varianza xs = media [(x-m)^2 | x <-xs]
 where   m = media (xs)

media xs =  sum(xs) / genericLength xs