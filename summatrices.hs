import Data.List
ma=[[1.0,2.0,1.0,0.0],[1.0,1.0,1.0,1.0],[2.0,2.0,2.0,2.0],[2.0,2.0,3.0,4.0]]
mb=[[2.0,2.0,2.0,2.0],[1.0,1.0,1.0,1.0],[-1.0,-1.0,-1.0,-1.0],[0.0,0.0,0.0,0.0]]
va=[1.0,2.0,3.0,4.0]
vb=[2.0,2.0,2.0,2.0]
summat::[[Double]]->[[Double]]->[[Double]]
summat [] matB = []
summat matA []= []
summat matA matB= (sumvec (head matA) (head matB)):(summat (tail matA) (tail matB))

sumvec::[Double]->[Double]->[Double]
sumvec [] vecB = []
sumvec vecA [] = []
sumvec (x:xs) (xb:xsb) = (x+xb):(sumvec xs xsb)
