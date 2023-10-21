#ejercicio 11
diagnostico=function(sens,espec,prev){
  VPP=(sens*prev)/((sens*prev)+(1-espec)*(1-prev))
  VPN=(espec*(1-prev))/(espec*(1-prev)+(1-sens)*prev)
  return((c(VPP,VPN)))
}
#ejemplo
diagnostico(0.85,0.98,0.012)

#Podemos alcanzar un valor predictivo positivo igual a 1 para algun
#valor de sensibilidad?? (manteniendo prev y espec)
diagnostico(1,0.98,0.012)
#Y si cambiamos espec y sens a 1??
diagnostico(1,1,0.012)
#Calcular los valores predictivos si la prev es 5% (manteniendo sens y espec)
diagnostico(0.85,0.98,0.05)

#Curva rock METODO 1
c=seq(50,250,by=0.1)
FVP=1-pnorm(c,mean=140,sd=30)
FFP=1-pnorm(c,mean=90,sd=10)
plot(FFP,FVP,type="l")
abline(a=0,b=1,lty=3)

#Curva rock METODO 2
p=seq(0,1,by=0.001)
Rocp=1-pnorm(qnorm(1-p,mean=90,sd=10),mean=140,sd=30)
plot(p,Rocp,type="l")
AUC=sum(Rocp)*0.001
#indice de youden, grafica de abajo
M=max(Rocp-p)
#valor pto de corte asociado
which.max(Rocp-p) #35 es donde se encuentra el macx de la curva
#p[35] : 1-especif, 0.034
#Rocp(35) sensibilidad,  0.855
qnorm(1-p[35],mean=90,sd=10)
qnorm(1-Rocp[35],mean=140,sd=30)
#nos debe dar lo mismo, seria el punto de corte
