
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