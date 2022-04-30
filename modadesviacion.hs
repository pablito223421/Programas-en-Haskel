import Data.List --Importacion que traabaja con listas--
import Data.Map (Map)--exporta funciones que colisionan con las de Prelude y Data.List--
import qualified Data.Map as Numeros

cantidadrepeticiones :: Ord a => [a] -> Map a Int --Ordenmaiento de mapas a enteros--
cantidadrepeticiones  []     = Numeros.empty--Analiza cuantas veces se esta repitiendo el numero--
cantidadrepeticiones  (x:xs) = Numeros.insertWith (+) x 1 (cantidadrepeticiones  xs)--cuenta cantidad de veces que se repite el numero en la lista--
 --ejemplo [1,2,3,4,5,3,2,1,3]; [(1,2),(2,2),(3,3),(4,1),(5,1)]--


moda :: Ord a => [a] -> [a]
moda xs = Numeros.keys (Numeros.filter (==m) d)--
    where d = cantidadrepeticiones  xs  --Selecciona la lista de numeros--
          m = maximum (Numeros.elems d)--guarda el numero que mas se repite--



 