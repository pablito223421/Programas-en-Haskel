busquedabinaria:: [Int]->Int->Int->Int-> Maybe Int
busquedabinaria xs value lon pos
               |pos < lon = Nothing
               |xs !! mitad > value = busquedabinaria xs value lon (mitad -1)
               |xs !! mitad < value = busquedabinaria xs value (mitad +1) pos
               |otherwise = mitad
               where 
               	mitad= lon + ((pos- lon) `div` 2)
