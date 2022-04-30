frecuencia :: (Double)->[Double]
frecuencia f = 
	                  let frecuenciatotal = sum f
                      frecrelativa = [ (f / frecuenciatotal) | f<-f]
                      frecacum  = scanl (\acc x -> acc + (x))0 f
                      frecrelativaacum =[ (frecacum / frecuenciatotal) | frecacum<-frecacum]
                      sumafrecrelativa = sum frecrelativa
                      sumafrecrelativaacum = sum frecrelativaacum
                      sumafrectotal = sumafrecrelativa/sumafrecrelativaacum
              in  (sumafrectotal)
----f=[7,6,8,6,5,4,4,2]