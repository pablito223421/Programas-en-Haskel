busquedasecuencial:: Ord  num=> [num] -> num -> Bool
busquedasecuencial[] _ = False
busquedasecuencial (x:xs)elemento
                |x== elemento = True
                |True = busquedasecuencial xs elemento