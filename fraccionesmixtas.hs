sumafraccionmixta1::(Integer,Integer,Integer)->(Integer,Integer,Integer)->(Integer,Integer,Integer)

sumafraccionmixta1 (e,a,c) (f,b,d) = let nummul1=c
                                         nummul2=d
                                         divisor= lcm nummul1 nummul2
                                         dividendo1=(nummul1*e)+a
                                         dividendo2=(nummul2*f)+b
                                         dividendo=dividendo1+dividendo2
                                         entera= div dividendo divisor 
                                         residuo= mod dividendo divisor 
                                      in (entera,residuo,divisor)

sumafraccionmixta2::(Integer,Integer,Integer)->(Integer,Integer,Integer)->(Integer,Integer)

sumafraccionmixta2 (e,a,c) (f,b,d) = let nummul1=c
                                         nummul2=d
                                         divisor= lcm nummul1 nummul2
                                         dividendo1=(nummul1*e)+a
                                         dividendo2=(nummul2*f)+b
                                         dividendo=dividendo1+dividendo2
                                         entera= div dividendo divisor 
                                         residuo= mod dividendo divisor 
                                      in (dividendo,divisor)


restafraccionmixta1::(Integer,Integer,Integer)->(Integer,Integer,Integer)->(Integer,Integer,Integer)

restafraccionmixta1 (e,a,c) (f,b,d) = let nummul1=c
                                          nummul2=d
                                          divisor= lcm nummul1 nummul2
                                          dividendo1=(nummul1*e)+a
                                          dividendo2=(nummul2*f)+b
                                          dividendo=dividendo1-dividendo2
                                          entera= div dividendo divisor 
                                          residuo= mod dividendo divisor 
                                      in (entera,residuo,divisor)

restafraccionmixta2::(Integer,Integer,Integer)->(Integer,Integer,Integer)->(Integer,Integer)

restafraccionmixta2 (e,a,c) (f,b,d) = let nummul1=c
                                          nummul2=d
                                          divisor= lcm nummul1 nummul2
                                          dividendo1=(nummul1*e)+a
                                          dividendo2=(nummul2*f)+b
                                          dividendo=dividendo1-dividendo2
                                          entera= div dividendo divisor 
                                          residuo= mod dividendo divisor 
                                     in (dividendo,divisor)


multiplicacionfraccionmixta1::(Integer,Integer,Integer)->(Integer,Integer,Integer)->(Integer,Integer,Integer)

multiplicacionfraccionmixta1 (e,a,c) (f,b,d)  = let nummul3=c
                                                    nummul4=d
                                                    divisor= nummul3*nummul4
                                                    dividendo1=(nummul3*e)+a
                                                    dividendo2=(nummul4*f)+b
                                                    dividendo=dividendo1*dividendo2
                                                    entera= div dividendo divisor 
                                                    residuo= mod dividendo divisor 
                                               in (entera,residuo,divisor)

multiplicacionfraccionmixta2::(Integer,Integer,Integer)->(Integer,Integer,Integer)->(Integer,Integer)

multiplicacionfraccionmixta2 (e,a,c) (f,b,d)  = let nummul3=c
                                                    nummul4=d
                                                    divisor= nummul3*nummul4
                                                    dividendo1=(nummul3*e)+a
                                                    dividendo2=(nummul4*f)+b
                                                    dividendo=dividendo1*dividendo2
                                                    entera= div dividendo divisor 
                                                    residuo= mod dividendo divisor 
                                               in (dividendo,divisor)

divisionfraccionmixta1::(Integer,Integer,Integer)->(Integer,Integer,Integer)->(Integer,Integer,Integer)

divisionfraccionmixta1 (e,a,c) (f,b,d) = let divisor1=c
                                             divisor2=d
                                             dividendo1= (divisor1*e) + a
                                             dividendo2= (divisor2*f) + b
                                             divisor= dividendo2*divisor1
                                             dividendo=dividendo1*divisor2
                                             entera= div dividendo divisor 
                                             residuo= mod dividendo divisor 
                                         in (entera,residuo,divisor)

divisionfraccionmixta2::(Integer,Integer,Integer)->(Integer,Integer,Integer)->(Integer,Integer)

divisionfraccionmixta2 (e,a,c) (f,b,d) = let divisor1=c
                                             divisor2=d
                                             dividendo1= (divisor1*e) + a
                                             dividendo2= (divisor2*f) + b
                                             divisor= dividendo2*divisor1
                                             dividendo=dividendo1*divisor2
                                             entera= div dividendo divisor 
                                             residuo= mod dividendo divisor 
                                         in (dividendo,divisor)