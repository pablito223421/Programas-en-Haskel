sumaprom []=0
sumaprom(x:xs)=x+ sumaprom(xs)

valores[]=0
valores(x:xs)=1+ valores(xs)

promedionum(xs)= (sumaprom(xs)/valores(xs))