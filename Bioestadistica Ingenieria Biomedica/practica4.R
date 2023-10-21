#ejercicio 13 tema 2
c=seq(0,10,by=0.1)

FFP=1-pexp(c,rate=1) #1-especificidad
#rate hace referencia al grado de la exponencial
FVP=1-pexp(c-1,rate=1) #sensibilidad
plot(FFP,FVP,type='l')
abline(a=0,b=1,lty=2)

#segunda forma
p=seq(0,1,by=0.01)
ROCp=1-pexp(qexp(1-p,rate=1)-1,rate=1)
plot(p,ROCp,type='l')
abline(a=0,b=1,lty=2)

#continuamos en el script tema 3
 
