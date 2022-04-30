

{-13-003-0009 Hernández Hernández Juan Pablo Grupo:301 Lenguajes de Programación Ejercicios 1 y 2-}

linea x = if x>0
then do putStr("+") --Imprime el +
        linea(x-1)  --al numero de lineas se le resta 1
else putStr("\n")

linea2 x = if x>0
then do putStr("+")  --Imprime el +
        linea(x)      --numero de lineas mas 1
else putStr("\n")

asterisco x = if x>0
then do asterisco(x-1)  --definir funcion recursiva
        linea(x-1)      --restar 1 a x almacenando la linea
else putStr("\n")

asterisco2 x = if x>0
then do linea (x-1)       --restar 1 a x almacenando la linea
        asterisco2(x-1)   --definir funcion recursiva
else putStr("\n")


asterisco3 x = if x>0
then do asterisco3(x-1)   --definir funcion recursiva
        linea2(x-1)       --restar 1 a x almacenando la linea2
else putStr("\n")

{-Imprimirtriangulo1: Imprime el primer ejercicio de la piramide-}
imprimirtriangulo1 x= if x>0
then do asterisco (x-1)  --restar 1 a x almacenando a asterisco
        asterisco2(x)     --nombre de la recursion asterisco2
else putStr("\n")

{-Imprimirtriangulo2: Imprime el segundo ejercicio de juntar los triangulos-}
imprimirtriangulo2 x = if x>0
then do asterisco2(x)    --nombre de la recursion asterisco2
        asterisco3(x-1)  --nombre de la recursion asterisco3-1
else putStr("\n")