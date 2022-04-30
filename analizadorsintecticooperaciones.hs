import Text.ParserCombinators.ReadP-- Libreria de analizador en paralelo, no matiene cadena de entrada
import Control.Applicative-- libreria para muchos usos, por ejemplo. el análisis libre de contexto pueden realizar análisis de cálculos
import Data.Char ----libreria que  representan puntos del código Unicode (Ascii)

{-
S1 := T +S  | T
T := F * T   | F
F := (S ) | N 
N:=ND|D
D := [0..9]+
-}

--notpolaca expresion "3+4*(3+7)"
--Just (OpeArit "+" (Numero 3) (OpeArit "*" (Numero 4) (OpeArit "+" (Numero 3) (Numero 7))))


data Exp
  = Numero Int
  |OpeArit String  Exp Exp
  | Identificador String
  deriving Show

notpolaca:: ReadP a -> String -> Maybe a --Lee un Parser a hacia un caracter y recibe un encapsulamiento a
notpolaca parser entrada =
  let res = readP_to_S parser (remueveEspacios entrada)  
  in
    if null res || snd (last res) /= "" then Nothing -- Aparece un mensaje que no existe si es nulo o no es un )
      -- y devuelve el segundo numero
    else Just (fst (last res))--Devuelve el primer componente numerico del ultimo componente de la expresion res

expresion :: ReadP Exp
expresion  = do
  (suma <|> termino) ---Leera si es una suma (+) o un Termino (multiplicacion o factor)

suma :: ReadP Exp
suma = do
  izquierda<- termino
  op <- (string "+")
  derecha <- expresion ------------------Termino que leera si es a la izquierda o a la derecha antes de un digito
  return (OpeArit op izquierda derecha)

termino :: ReadP Exp
termino = do
  (multiplicacion <|> factor)-----Leera si la expresion debe tener un *  o si puede irse a funcion factor

multiplicacion :: ReadP Exp
multiplicacion = do
  izquierda<- factor                  
  op <- (string "*")---------------------------Termino que tiene como termino el signo *-------------------
  derecha <- termino
  return (OpeArit op izquierda derecha)

factor :: ReadP Exp
factor = do
  (parentesis<|> digito <|> identificador)-----------Puede leer un parentesis y comenzar con una expresion, leer un digito o un i
  --identificador despues de ( y un digito  

parentesis:: ReadP Exp
parentesis= do
  string "("
  inside <- expresion ---Pueden llevarlo a travez de la expresion de dos operaciones suma o multimplicacion
  string ")"
  return inside

digito :: ReadP Exp
digito = do
  str <- munch1 (\c -> c >= '0' && c <= '9') -------Leera si es del 0 al 9
  return (Numero(read str))

identificador :: ReadP Exp
identificador = do
  str <- munch1 isAlpha ----------------------Leera los caracteres de operadores aritmeticos o digitos
  return (Identificador str)

remueveEspacios :: String -> String
remueveEspacios entrada = case entrada of
  "" -> ""
  c:"" -> if isSpace c then "" else c:""
  c:cs -> if isSpace c then remueveEspacios cs else c : (remueveEspacios cs)----Si hay espacio en la expresion automaticamente los elimina
-----------------------NotacionPolaca
  
data Expresion = D Int| Expresion:+Expresion  |Expresion:*Expresion ---Datos de tipo expresion pero para el resultado numerico

operacion:: Expresion->Int ----Tipo expresion a un Entero
-----D digito x el numero
operacion (D x)   = x
operacion (dig1:+dig2)=operacion dig1 + operacion dig2 
operacion (dig1:*dig2)=operacion dig1 * operacion dig2
--operacion (D 3 :+ ((D 4) :* (D 3 :+ D 7)))