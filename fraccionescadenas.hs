import Data.Char

partir :: String ->Char-> [String]
partir [] c = [""]
partir (x:xs) c
        | x == c = "" : partir xs c
        | otherwise = (x : head (partir xs c)) :( tail (partir xs c))


operafraccion::(Integer,Integer)->(Integer,Integer)->([(Integer,Integer)],[(Integer,Integer)],[(Integer,Integer)],[(Integer,Integer)])
operafraccion (a,c) (b,d) = let nummul1 = c
                                nummul2 = d
                                divisor = lcm nummul1 nummul2
                                dividendo1 = (divisor `div` nummul1)*(a)
                                dividendo2 = (divisor `div` nummul2)*(b)
                                dividendosum = dividendo1+dividendo2
                                dividendores = dividendo1-dividendo2
                                divisormul=  nummul1 * nummul2
                                nummul3 = a
                                nummul4 = b
                                dividendomul = nummul3*nummul4
                                dividendodiv = nummul3 *nummul2
                                divisordiv = nummul1 *nummul4
                             in ([(dividendosum, divisor)],
                                [(dividendores, divisor)],
                              [(dividendomul,divisormul)],
                              [(dividendodiv,divisordiv)])
                         


