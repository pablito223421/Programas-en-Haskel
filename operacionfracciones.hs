sumafraccion::(Integer,Integer)->(Integer,Integer)->(Integer,Integer)

sumafraccion (a,c) (b,d) = let nummul1=c
                               nummul2=d
                               divisor= lcm nummul1 nummul2
                               dividendo1=(divisor `div` nummul1)*(a)
                               dividendo2=(divisor `div` nummul2)*(b)
                               dividendo=dividendo1+dividendo2
                           in (dividendo,divisor)

restafraccion::(Integer,Integer)->(Integer,Integer)->(Integer,Integer)

restafraccion (a,c) (b,d) = let nummul1=c
                                nummul2=d
                                divisor= lcm nummul1 nummul2
                                dividendo1=(divisor `div` nummul1)*(a)
                                dividendo2=(divisor `div` nummul2)*(b)
                                dividendo=dividendo1-dividendo2
                            in  (dividendo,divisor)

multiplicacionfraccion::(Integer,Integer)->(Integer,Integer)->(Integer,Integer)

multiplicacionfraccion (a,c) (b,d)  = let nummul3=c
                                          nummul4=d
                                          divisor=  nummul3 * nummul4
                                          nummul1=a
                                          nummul2=b
                                          dividendo=nummul1*nummul2
                                      in (dividendo,divisor)

divisionfraccion::(Integer,Integer)->(Integer,Integer)->(Integer,Integer)

divisionfraccion (a,c) (b,d) = let divsor1=c
                                   divsor2=b
                                   divisor=  divsor1 *divsor2
                                   divendo1=a
                                   divendo2=d
                                   dividendo=divendo1*divendo2
                               in (dividendo,divisor)