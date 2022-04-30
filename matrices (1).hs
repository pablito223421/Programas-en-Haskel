import Data.List

--Tarea
a=[[2.0,-2.0,-2.0],[-2.0,-2.0,2.0],[-4.0,8.0,-4.0]]
b=[[3.0,1.0,5.0],[0.0,-2.0,6.0],[3.0,-3.0,7.0]]
c=[[1.0,2.0],[2.0,-1.0]]
d=[[-1.0,-1.0],[5.0,6.0]]
x=[[1.0,2.0,3.0],[3.0,2.0,1.0],[1.0,0.0,1.0]]
y=[[1.0,2.0,3.0,4.0],[4.0,3.0,2.0,1.0],[1.0,1.0,0.0,1.0],[1.0,1.0,1.0,1.0]]
ec=[[4,5,3],[6,-10,1]]

--------------------------------------------------------------------------------------------------------------------------------------
--suma de matrices
summat::[[Double]]->[[Double]]->[[Double]]
summat [] matB = []
summat matA []= []
summat matA matB = (sumvec (head matA) (head matB)):(summat (tail matA) (tail matB))

sumvec::[Double]->[Double]->[Double]
sumvec [] vecB = []
sumvec vecA [] = []
sumvec (x:xs) (xb:xsb) = (x+xb):(sumvec xs xsb)

--------------------------------------------------------------------------------------------------------------------------------------
--resta de matrices
resmat::[[Double]]->[[Double]]->[[Double]]
resmat [] matB = []
resmat matA []= []
resmat matA matB = (resvec (head matA) (head matB)):(resmat (tail matA) (tail matB))

resvec::[Double]->[Double]->[Double]
resvec [] vecB = []
resvec vecA [] = []
resvec (x:xs) (xb:xsb) = (x-xb):(resvec xs xsb)

--------------------------------------------------------------------------------------------------------------------------------------
--multiplicación de matrices
multMat::[[Double]]->[[Double]]->[[Double]]
multMat [] matB = []
multMat matA []= []
multMat matA matB = multAux matA (traspuesta matB):multMat (tail matA) matB

multAux::[[Double]]->[[Double]]->[Double]
multAux [] matB = []
multAux matA []= []
multAux matA matB = multvec (head matA) (head matB):multAux matA (tail matB)

multvec::[Double]->[Double]->Double
multvec [] vecB = 0.0
multvec vecA [] = 0.0
multvec (x:xs) (xb:xsb) = (x*xb)+(multvec xs xsb)

traspuesta::[[a]] -> [[a]]
traspuesta ([]:_) = []
traspuesta xss = primeros xss : traspuesta (restos xss)

primeros xss = [head xs | xs <- xss] 
 
restos xss   = [tail xs | xs <- xss]

--------------------------------------------------------------------------------------------------------------------------------------
--potencia
potenciaAux matA matB 0 = matB
potenciaAux matA matB contador =  potenciaAux matA (multMat (matA) (matB)) (contador-1)

potencia matA pot = potenciaAux matA matA (pot-1)

--------------------------------------------------------------------------------------------------------------------------------------
--determinante
determinante :: [[Double]]->Double
determinante matA = if (length matA)==2 then sum(determinanteAux(matrizD(concatenaCero matA) (length matA) 0)) 
                                             - sum(determinanteAux(matrizIZ(concatenaCero matA) (length matA) ))
                    else sum(determinanteAux(matrizD (duplicaDatos matA (length matA)) (length matA) 0)) 
                         - sum(determinanteAux(matrizIZ (duplicaDatos matA (length matA)) (length matA)))

concatenaCero matA=[x++[0] |x<-matA]

determinanteAux [] = []
determinanteAux matA =producto (head matA):determinanteAux (tail matA)

producto lista= product[x | x<-lista]

duplicaDatos :: [[a]] -> Int -> [[a]]
duplicaDatos [] y = []
duplicaDatos (x:xs) y =  [x ++ take (y-1) x] ++ duplicaDatos (xs) y

matrizDiagonalIZD [] contador = []
matrizDiagonalIZD (x:xs) contador  = x !!contador : matrizDiagonalIZD xs (contador+1)

matrizD [] longitud contador = []
matrizD (x:xs) longitud contador = if contador /= longitud then 
                                           [matrizDiagonalIZD (x:xs) contador] ++ matrizD (x:xs) longitud (contador+1) 
                                        else [] 

resta [] contador = []
resta (x:xs) contador  = x !!contador : resta xs (contador-1)

matrizDiagonalDIZ [] longitud contador = []
matrizDiagonalDIZ (x:xs)  longitud contador = if longitud /= 0 then 
                                                    [resta (x:xs) contador] ++ matrizDiagonalDIZ (x:xs) (longitud-1) (contador-1) 
                                                        else [] 
                                        
matrizIZ (x:xs) longitud = matrizDiagonalDIZ (x:xs) longitud ((length x)-1)

--------------------------------------------------------------------------------------------------------------------------------------
--matriz adjunta
adjunta [] = []
adjunta matA = adjuntaDos (head matA):adjunta (tail matA)

adjuntaDos matA = [x*(-1) |x <-matA]

--------------------------------------------------------------------------------------------------------------------------------------
--división de matrices
divMat::[[Double]]->[[Double]]->[[Double]]
divMat matA matB= multMat matA (inversa matB)

division [] a = []
division (x:xs) a  = [map (/a) x] : division xs a

--------------------------------------------------------------------------------------------------------------------------------------
--matriz inversa
inversa::[[Double]]->[[Double]]
inversa matA = inversaAux (traspuesta (adjunta matA)) (determinante matA)

inversaAux::[[Double]]->Double->[[Double]]
inversaAux [] determinante= []
inversaAux matA determinante = inversaDiv (head matA) determinante : inversaAux (tail matA) determinante

inversaDiv matA determinante = [x/determinante | x<-matA]

--------------------------------------------------------------------------------------------------------------------------------------
--Resolver ecuaciones simultáneas por Gauss Jordan
ecuaciongaussreducido :: [[Double]]->[[Double]]
ecuaciongaussreducido matriz = ultimafila $ foldl eliminafila matriz [0..length matriz-1] where
 
 swap xs a b
  | a > b = swap xs b a --intercambia el elemento en la posición a con el elemento en la posición b swap xs a b swap xs a b
  | a == b = xs
  | a < b = let
  (p1,p2) = splitAt a xs
  (p3,p4) = splitAt (b-a-1) (tail p2)
  in p1 ++ [xs!!b] ++ p3 ++ [xs!!a] ++ (tail p4)
 eliminafila matriz1 r = let
  elementonoescero = head $ filter (\x -> matriz1 !! x !! r /= 0) [r..length matriz1-1] --El primer elelemto distinto de cero.
  matriz2 = swap matriz1 r elementonoescero --intercambio de fila con la matriz
  filas = matriz2 !! r
  fila1 = map (\x -> x / (filas !! r)) filas --Hace que tenga 1 como coefinciente
  subfila nr = let k = nr!!r in zipWith (\a b -> k*a - b) fila1  nr --resta nr de la fila 1 mientras se multiplica
  siguientefila = map subfila $ drop (r+1) matriz2 -- aplicar subfila a todas las filas de abajo
  in take r matriz2 ++ [fila1] ++ siguientefila --Concatena y se repite en las listas
 
 ultimafila matriz' = let
  a = init matriz'; filas = last matriz'; z = last filas; nz = last (init filas)
  in a ++ [init (init filas) ++ [1, z / nz]]

sustitucion ::[[Double]]->[Double] --Resuelve la ecuacion de la matriz por medio de la sustitucion
sustitucion matriz = foldr siguientemat [last (last matriz)] (init matriz) where
 
 siguientemat fila found = let
  subpart = init $ drop (length matriz - length found) fila
  solucion= last fila - sum (zipWith (*) found subpart)
  in solucion: found

resolverecuacion ::[[Double]] -> [Double]
resolverecuacion = sustitucion . listamatriz . ecuaciongaussreducido

listamatriz ::[[Double]]->[[Double]]
listamatriz m = map listafilas m

listafilas r =
    let 
     avanza = head $ dropWhile (\x -> x == 0) r
    in
      map (\x -> x / avanza) r

      --ecuaciongaussreducido [[3.0,2.0,4.0,1.0], [5.0,-1.0,-3.0,-7.0], [4.0,3.0,1.0,2.0] ]
      --resolverecuacion [[3.0,2.0,4.0,1.0], [5.0,-1.0,-3.0,-7.0], [4.0,3.0,1.0,2.0] ]
      --ecuaciongaussreducido [[2.0,3.0,1.0,1.0], [3.0,-2.0,-4.0,-3.0], [5.0,-1.0,-1.0,4.0] ]
      --resolverecuacion[[2.0,3.0,1.0,1.0], [3.0,-2.0,-4.0,-3.0], [5.0,-1.0,-1.0,4.0] ]

--------------------------------------------------------------------------------------------------------------------------------------
--Resolver ecuaciones simultáneas usando Determinantes
ecDeterminantes::[[Double]]->(Double,Double)
ecDeterminantes vecA = let detX = determinante (determinantesAux vecA) / determinante (determinantesS vecA)
                           detY = determinante (determinantesAux(duplicaDatos vecA 2)) / determinante (determinantesS vecA)
                       in(detX,detY)  
                       
determinantesS [] = []
determinantesS vecA = take 2 (head vecA): determinantesS (tail vecA)

determinantesAux [] = []
determinantesAux vecA = take 2 (reverse(head vecA)): determinantesAux (tail vecA)


