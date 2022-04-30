import Data.List
ma=[[1.0,2.0,1.0,0.0],[1.0,1.0,1.0,1.0],[2.0,2.0,2.0,2.0],[2.0,2.0,3.0,4.0]]
mb=[[2.0,2.0,2.0,2.0],[1.0,1.0,1.0,1.0],[-1.0,-1.0,-1.0,-1.0],[0.0,0.0,0.0,0.0]]
va=[1.0,2.0,3.0,4.0]
vb=[2.0,2.0,2.0,2.0]

summat::[[Int]]->[[Int]]->[[Int]]
summat [] matB = []
summat matA []= []
summat matA matB= (sumvec (head matA) (head matB)):(summat (tail matA) (tail matB))
                  
sumvec::[Int]->[Int]->[Int]
sumvec [] vecB = []
sumvec vecA [] = []
sumvec (x:xs) (xb:xsb) = (x+xb):(sumvec xs xsb)


-------summat [[1,2,3],[1,1,-1],[-1,0,2]] [[0,-1,-1],[-2,-4,1],[0,1,1]]


-------------------------------------------Suma de Matrices---------------------------------------------------------- 

resmat::[[Int]]->[[Int]]->[[Int]]
resmat [] matB = []
resmat matA []= []
resmat matA matB= (resvec (head matA) (head matB)):(resmat (tail matA) (tail matB))
                
resvec::[Int]->[Int]->[Int]
resvec [] vecB = []
resvec vecA [] = []
resvec (x:xs) (xb:xsb) = (x-xb):(resvec xs xsb)


--------restmat [[1,2,3],[1,1,-1],[-1,0,2]] [[0,-1,-1],[-2,-4,1],[0,1,1]]


---------------------------------------Resta de Matrices------------------------------------------------------------

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

--------multimatriz [[1,2,3],[1,1,-1],[-1,0,2]] [[0,-1,-1],[-2,-4,1],[0,1,1]]

-------------------------------------------Multiplicacion de Matrices-----------------------------------------------                                  
potsum::[Int]->[Int]->Int 
potsum [] []=0 
potsum (x:xs) (y:ys)|k==m=x*y+potsum xs ys 
     |otherwise=0 
     where k=length (x:xs) 
           m=length (y:ys)



potencia::[Int]->[[Int]]->Int->[Int] 
potencia [] _ _=[] 
potencia (xs) (y:yss) a|a<=length y=d:potencia(xs) (y:yss) (a+1) 
     |otherwise=[] 
     where k=posicion (y:yss) a 
           d=potsum xs k

potmatriz::[[Int]]->[[Int]]->[[Int]] 
potmatriz [] qss=[] 
potmatriz (p:pss) qss|length p==length qss=m:potmatriz pss qss 
     |otherwise=error "el numero de columnas debe ser igual al numero de filas" 
     where m=potencia p (qss) 1 
 ----potmatriz [[0,1,0],[1,0,1],[0,1,0]][[0,1,0],[1,0,1],[0,1,0]] 
 ----[[1,0,1],[0,2,0],[1,0,1]]

--------------------------------------Potencia de Matrices....................................................................