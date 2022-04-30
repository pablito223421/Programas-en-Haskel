eclineal::[Double]->[Double]->(Double,Double)
eclineal l1 l2 = do 
                   --let sigmxy = sum[x*y,(x,y)<-zip l1 l2]
                   let sigmxy = sum $ map (\(a,b)-> a*b) (zip l1 l2)

                   let sigmx = sum l1
                   let sigmy = sum l2
                   let sigmx2 =  foldl (\acc x-> acc +(x^2)) 0 l1
                   let n = fromIntegral(length l1)
                   
    
                   let a = (n*sigmxy-sigmx*sigmy)/(n*sigmx2-(sigmx^2))
                   let b = (sigmy-(a*sigmx))/(n)
                   (a,b)
--  x  = [100,90,80,45,50,50,60,40,25,20]
-- y = [3,5,9,10,20,21,24,24,27,35]
divideByTen :: (Floating a) => a -> a  
divideByTen = (/10)  

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
