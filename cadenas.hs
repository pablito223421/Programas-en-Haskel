--a = "1/2+2/3 8/5r-7/9 15/8÷2/5 3/5x9/5"
import Data.Char

partir :: String ->Char-> [String]
partir [] c = [""]
partir (x:xs) c
       | x == c = "" : partir xs c
       | otherwise = (x : head (partir xs c)) :( tail (partir xs c))


cadena::[String]->[(Int,Int)]
cadena [] = []
cadena lista = do
                  let  operador=getoper bloque
                       numeros=partir bloque operador
                       frac1= partir (head numeros) '/'
                       frac2= partir (last numeros) '/'
                       num1=frac1!!0
                       den1=frac1!!1
                       num2=frac2!!0
                       den2=frac2!!1                                     
                  case operador of 
                     '+' ->  (sumafrac (stoInt(num1),stoInt(den1)) (stoInt(num2),stoInt(den2))):(cadena (tail lista))
                     'r' -> (restafrac (stoInt(num1),stoInt(den1)) (stoInt(num2),stoInt(den2))):(cadena (tail lista))
                     '÷' ->   (divfrac (stoInt(num1),stoInt(den1)) (stoInt(num2),stoInt(den2))):(cadena (tail lista))
                     'x' ->  (multfrac (stoInt(num1),stoInt(den1)) (stoInt(num2),stoInt(den2))):(cadena (tail lista))                
                  where               
                      bloque = head lista

processfrac::String->[(Int,Int)]
processfrac [] = []
processfrac lista = cadena (partir lista ' ')


getoper:: String->Char
getoper [] = '\NUL'
getoper (x:xs) 
     | isDigit x = getoper xs
     | x == '/'  = getoper xs
     | x == '-' = getoper xs
     | otherwise = x

stoInt::String->Int
stoInt ""  = 0
stoInt cadena 
         |head cadena=='-' = (convierte (reverse cadena) 0)*(-1)
         | otherwise = (convierte (reverse cadena) 0)

convierte::String->Int->Int            
convierte [] a = 0
convierte (x:xs) a  
           | x=='-' = 0
           | otherwise = digitToInt(x)*(10^a) + convierte xs (a+1)  

convertir::String->Int
convertir cadena = read cadena::Int

sumafrac (n1,d1) (n2,d2) =  let   dr=lcm d1 d2
                                  nr=(div dr d1)*n1+(div dr d2)*n2
                            in    reduce(nr,dr) 

restafrac (n1,d1) (n2,d2) = let   dr=lcm d1 d2
                                  nr=(div dr d1)*n1-(div dr d2)*n2
                            in    reduce(nr,dr)  

multfrac (n1,d1) (n2,d2) = let   dr=d1*d2
                                 nr=n1*n2
                           in    reduce(nr,dr)

divfrac (n1,d1) (n2,d2) = let   dr=d1*n2
                                nr=n1*d2
                          in    reduce(nr,dr)

reduce (n,d) = (n `div` (gcd n d),d `div` (gcd n d))
