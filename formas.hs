type Rango = (Int,Int)

longr::Rango->Int
longr z = (snd z)-(fst z)  


data Shape = Circle Float | Rect Float Float deriving (Show)

square :: Float -> Shape


square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div`n)

--a = Rect 4 4
--area a


--isNothing   
--fromJust


