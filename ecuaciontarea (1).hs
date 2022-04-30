eclineal::(Double,Double)->[Double,Double]
--  x  = [100,90,80,45,50,50,60,40,25,20]
-- y = [3,5,9,10,20,21,24,24,27,35]
eclineal l1 l2 = 
                   --let sigmxy = sum[x*y,(x,y)<-zip l1 l2]
                   --let   sigmxy = sum $ map (\(a,b)-> a*b) (zip l1 l2)
                     let ax    = sum  l1
                     let bi    = sum  l2
                    siglistax = [  (l1-(ax/10))^2 | l1 <-l1]
                    siglistay = [  (l2-(bi/10))^2 | l2 <-l2]
                    sigacumx  = sum  siglistax
                    sigacumy  = sum  siglistay
                    sigmullistaxy= zipWith (*) siglistax siglistay
                    sigsummullistaxy= sum sigmullistaxy
                    raizsigacumx=sqrt(sigacumx)
                    raizsigacumy=sqrt(sigacumy)
                    mulraizxy= raizsigacumx * raizsigacumy
                    r=sigsummullistaxy/mulraizxy
                 in  (r)

                divideByTen :: (Floating ax) => ax -> ax  
divideByTen = (/10)

                divideByTen :: (Floating bi) => bi -> bi  
divideByTen = (/10)   

zipWith' :: (ax -> bi -> r) -> [ax] -> [bi] -> [r]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
       