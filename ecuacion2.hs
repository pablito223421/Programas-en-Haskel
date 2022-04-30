eclineal::[Double]->[Double]->(Double,Double)
eclineal l1 l2 = 
                   --let sigmxy = sum[x*y,(x,y)<-zip l1 l2]
                   let sigmxy = sum $ map (\(a,b)-> a*b) (zip l1 l2)
                       sigmx = sum l1
                       sigmy = sum l2
                       sigmx2 =  foldl (\acc x-> acc +(x^2)) 0 l1
                       n = fromIntegral(length l1)
                       a = (n*sigmxy-sigmx*sigmy)/(n*sigmx2-(sigmx^2))
                       b = (sigmy-(a*sigmx))/(n)
                   in (a,b)


divideByTen :: (Floating a) => a -> a  
divideByTen = (/10)  

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

estacion mes = case mes of    
                  12 -> "Invierno"
                  1 -> "Invierno"
                  2 -> "Invierno"
                  3 -> "Primavera"
                  4 -> "Primavera"
                  5 -> "Primavera"
                  6 -> "Verano"
                  7 -> "Verano"
                  8 -> "Verano"
                  9 -> "Otonho"
                  10 -> "Otonho"
                  11 -> "Otonho"
                  _ -> "Error"

season mes = case mes of    
                 m | m == 12||m == 1||m == 2 -> "Invierno"
                 m | m >= 3 && m <= 5 -> "Primavera"
                 m | m >= 6 && m <= 8 -> "Invierno"
                 m | m >= 9 && m <= 11 -> "Otonho"
                 m | otherwise -> "Error"
                  

                                 
                                    
