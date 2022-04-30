module Fracciones
(sumafrac,
restafrac,
multfrac,
divfrac,
reduce)
where

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
