data Arbol a = ArbolVacio | Nodo a (Arbol a) (Arbol a) deriving (Show, Read, Eq)  

insarbolV :: a -> Arbol a  
insarbolV x = Nodo x ArbolVacio ArbolVacio  

insarbol :: (Ord a) => a -> Arbol a -> Arbol a  
insarbol x ArbolVacio = insarbolV x  
insarbol x (Nodo a izq der)   
    | x == a = Nodo x izq der  
    | x < a  = Nodo a (insarbol x izq) der  
    | x > a  = Nodo a izq (insarbol x der)


postorden :: Arbol a -> [a]
postorden ArbolVacio = []
postorden (Nodo  x izq der) = postorden izq ++ postorden der ++ [x]

preorden :: Arbol a -> [a]
preorden ArbolVacio = []
preorden (Nodo x izq der) = x : (preorden izq ++ preorden der)

inorden :: Arbol a -> [a]
inorden ArbolVacio = []
inorden (Nodo x izq der) = inorden izq ++ (x : inorden der)

--z = Nodo 10 ArbolVacio ArbolVacio
--z = Nodo 10 (Nodo 4 ArbolVacio (Nodo 5 ArbolVacio ArbolVacio)) (Nodo 3 ArbolVacio ArbolVacio) 
--z = foldr insarbol ArbolVacio [3,11,1,10]
