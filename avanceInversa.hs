
--duplicaDatos [[1,2,3],[4,5,6],[7,8,9]] 3
duplicaDatos :: [[a]] -> Int -> [[a]]
duplicaDatos [] y = []
duplicaDatos (x:xs) y =  [x ++ take (y-1) x] ++ duplicaDatos (xs) y

suma [] contador = []
suma (x:xs) contador  = x !!contador : suma xs (contador+1)


--sumaOriginal a 3 0
sumaOriginal [] longitud contador = []
sumaOriginal (x:xs) longitud contador = if contador /= longitud then [suma (x:xs) contador] ++ sumaOriginal (x:xs) longitud (contador+1) else [] 


multismum::[Int]->[Int]->Int 
multismum [] []=0 
multismum (x:xs) (y:ys)|k==m=x*y+multismum xs ys 
     |otherwise=0 
     where k=length (x:xs) 
           m=length (y:ys)

posicion::[[Int]]->Int->[Int] 
posicion [] _=[] 
posicion (a:ass) e=(a!!(e-1)):posicion ass e

multipl::[Int]->[[Int]]->Int->[Int] 
multipl [] _ _=[] 
multipl (xs) (y:yss) a|a<=length y=d:multipl (xs) (y:yss) (a+1) 
     |otherwise=[] 
     where k=posicion (y:yss) a 
           d=multismum xs k

multimatriz::[[Int]]->[[Int]]->[[Int]] 
multimatriz [] qss=[] 
multimatriz (p:pss) qss|length p==length qss=m:multimatriz pss qss 
     |otherwise=error "el numero de columnas debe ser igual al numero de filas" 
     where m=multipl p (qss) 1                                     


multismumpot::[Int]->[Int]->Int 
multismumpot [] []=0 
multismumpot (x:xs) (y:ys)|k==m=x*x+multismumpot xs ys 
     |otherwise=0 
     where k=length (x:xs) 
           m=length (y:ys)

multimatrizpot::[[Int]]->[[Int]]->[[Int]] 
multimatrizpot [] qss=[] 
multimatrizpot (p:pss) qss|length p==length qss=m:multimatrizpot pss qss 
     |otherwise=error "el numero de columnas debe ser igual al numero de filas" 
     where m=multipl p (qss) 1                                     


