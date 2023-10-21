####################################
#
# TEMA 6: TÉCNICAS MULTIVARIANTES
#
####################################




# 6.2. Técnicas descritivas multivariantes
##########################################

# 6.2.1. Representacións gráficas con datos multivariantes
##########################################################

mundo2016 <- read.csv("datos-mundo2016.csv",header=TRUE,dec=".",sep=",")
attach(mundo2016)
str(mundo2016)

# Boxplot: atención á orde dos niveis da variable "nivel.ingresos"
boxplot(taxa.fertilidade~nivel.ingresos)

# reordenación dos niveis:
nivel.ingresos <- factor(nivel.ingresos, levels = c("B","MB","MA","A"))
boxplot(taxa.fertilidade~nivel.ingresos)

#EJERCICIO 1 
#a)Podemos decir que cuanto mas baja es el nivel de ingresos,
#mas hijos se tienen por mujer
#b)
boxplot(esperanza.vida~nivel.ingresos) #mas alta cuanto mayor nivel de ingresos, mas recursos
#c)


# Gráfico de dispersión por pares
# (a opción "panel=panel.smooth" inclúe un estimador
# non paramétrico da función de regresión)
pairs(mundo2016[,7:10],panel=panel.smooth)



# 6.2.2. Vector de medias, matriz de varianzas-covarianzas
# e matriz de correlacións
##########################################################

# Vector de medias:
colMeans(mundo2016[,7:10])

# Matriz de varianzas-covarianzas
var(mundo2016[,7:10])

# Matriz de correlacións
cor(mundo2016[,7:10])



# 6.2.3. Distancia de Mahalanobis
#################################

datos.para.mahalanobis <- mundo2016[,c("taxa.fertilidade","esperanza.vida")]
M <- colMeans(datos.para.mahalanobis)
S <- cov(datos.para.mahalanobis)

# distancia de Mahalanobis de España
sqrt(mahalanobis(datos.para.mahalanobis[which(pais=="Spain"),],center=M,cov=S))

# distancias de Mahalanobis de todos os países
distancias.mahalanobis <- sqrt(apply(datos.para.mahalanobis,1,mahalanobis,center=M,cov=S))
cbind.data.frame(pais,codigo.pais,distancias.mahalanobis)

# Gráfico de dispersión cos códigos dos países
plot(taxa.fertilidade,esperanza.vida,type="n",xlab="taxa de fertilidade",ylab="esperanza de vida")
text(taxa.fertilidade,esperanza.vida,codigo.pais,cex=0.5)
abline(lm(esperanza.vida~taxa.fertilidade),lty=2)  # recta de regresión
points(mean(taxa.fertilidade),mean(esperanza.vida),pch=3,cex=1.5,lwd=2,col="red")  # vector de medias


# Gráfico de dispersión cos códigos dos países e a orde que lles corresponde
# pola distancia de Mahalanobis con respecto ao vector de medias
plot(taxa.fertilidade,esperanza.vida,type="n",xlab="taxa de fertilidade",ylab="esperanza de vida")
ord <- order(distancias.mahalanobis)
for (i in 1:length(ord)){
  text(taxa.fertilidade[ord[i]],esperanza.vida[ord[i]],paste(codigo.pais[ord[i]],"-",i,sep=""),cex=0.50)
}
abline(lm(esperanza.vida~taxa.fertilidade),lty=2)
points(mean(taxa.fertilidade),mean(esperanza.vida),pch=3,cex=1.5,lwd=2,col="red")

#EJERCICIO 2
boxplot(distancias.mahalanobis~nivel.ingresos)


detach(mundo2016)





# 6.4. Análise de Compoñentes Principais
##########################################


# 6.4.1. Cálculo das compoñentes principais
###########################################

decatlon <- read.csv(file="datos-decatlon.csv",head=TRUE,sep=",")
datos.para.PCA <- decatlon[,3:12]
PCA.decatlon <- princomp(datos.para.PCA,cor=TRUE)

# matriz de vectores unitarios
PCA.decatlon$loadings

# índices ("scores")
PCA.decatlon$scores

# correlacións entre as variables orixinais e as compoñentes principais
cor(datos.para.PCA,PCA.decatlon$scores)


# 6.4.2. Selección do número de compoñentes
###########################################

# Proporción de variabilidade explicada
summary(PCA.decatlon)

# Scree-plot
plot(PCA.decatlon)

# Gráficos de dispersión das tres primeiras compoñentes principais (Figura 6.7)

plot(PCA.decatlon$scores[,1],PCA.decatlon$scores[,2],xlab="primeira CP",ylab="segunda CP",type="n",frame=F)
text(PCA.decatlon$scores[,1],PCA.decatlon$scores[,2],decatlon$nome,cex=0.75)

plot(PCA.decatlon$scores[,1],PCA.decatlon$scores[,3],xlab="primeira CP",ylab="terceira CP",type="n",frame=F)
text(PCA.decatlon$scores[,1],PCA.decatlon$scores[,3],decatlon$nome,cex=0.75)

plot(PCA.decatlon$scores[,2],PCA.decatlon$scores[,3],xlab="segunda CP",ylab="terceira CP",type="n",frame=F)
text(PCA.decatlon$scores[,2],PCA.decatlon$scores[,3],decatlon$nome,cex=0.75)






# 6.5. Creación de grupos: Métodos cluster
##########################################


# 6.5.1. Método das K-medias
############################

climacidades <- read.csv("datos-climacidades.csv",sep=",",dec=".",header=TRUE)
attach(climacidades)
plot(hsol,prec,type="n",frame=F)
text(hsol,prec,codigo,cex=0.5)

# Mapas
#install.packages("ggplot2","maps","mapdata")
library(ggplot2)
library(maps)
library(mapdata)
map(regions=c("Spain","Portugal","Morocco","Algeria","France","Canary Islands"),xlim=c(-20,5),ylim=c(27,44))
points(lonxitude,latitude,cex=0.25,pch=1)
text(lonxitude,latitude+0.20,codigo,cex=0.50,pch=1)


# K=2 clusters
datos.para.cluster <- climacidades[,c("hsol","prec")]
Kmedias2 <- kmeans(datos.para.cluster,centers=2)
as.character(cidade)[Kmedias2$cluster==1]
as.character(cidade)[Kmedias2$cluster==2]

# gráfico de dispersión:
plot(hsol,prec,type="n",frame=F)
text(hsol[Kmedias2$cluster==1],prec[Kmedias2$cluster==1],codigo[Kmedias2$cluster==1],cex=0.5,col="red") 
text(hsol[Kmedias2$cluster==2],prec[Kmedias2$cluster==2],codigo[Kmedias2$cluster==2],cex=0.5,col="blue") 
points(mean(hsol[Kmedias2$cluster==1]),mean(prec[Kmedias2$cluster==1]),col="red",pch=3,lwd=2)  # centroid 1st group
points(mean(hsol[Kmedias2$cluster==2]),mean(prec[Kmedias2$cluster==2]),col="blue",pch=3,lwd=2) # centroid 2nd group

# mapa:
map(regions=c("Spain","Portugal","Morocco","Algeria","France","Canary Islands"),xlim=c(-20,5),ylim=c(27,45))
points(lonxitude[Kmedias2$cluster==1],latitude[Kmedias2$cluster==1],cex=0.25,pch=1,col="red")
points(lonxitude[Kmedias2$cluster==2],latitude[Kmedias2$cluster==2],cex=0.25,pch=3,col="blue")
text(lonxitude[Kmedias2$cluster==1],latitude[Kmedias2$cluster==1]+0.20,codigo[Kmedias2$cluster==1],cex=0.50,pch=1,col="red")
text(lonxitude[Kmedias2$cluster==2],latitude[Kmedias2$cluster==2]+0.20,codigo[Kmedias2$cluster==2],cex=0.50,pch=2,col="blue")

# K=3 clusters
Kmedias3 <- kmeans(datos.para.cluster,centers=3)
as.character(cidade)[Kmedias3$cluster==1]
as.character(cidade)[Kmedias3$cluster==2]
as.character(cidade)[Kmedias3$cluster==3]

# gráfico de dispersión:
plot(hsol,prec,type="n",frame=F)
text(hsol[Kmedias3$cluster==1],prec[Kmedias3$cluster==1],codigo[Kmedias3$cluster==1],cex=0.5,col="red") 
text(hsol[Kmedias3$cluster==2],prec[Kmedias3$cluster==2],codigo[Kmedias3$cluster==2],cex=0.5,col="blue") 
text(hsol[Kmedias3$cluster==3],prec[Kmedias3$cluster==3],codigo[Kmedias3$cluster==3],cex=0.5,col="green")
points(mean(hsol[Kmedias3$cluster==1]),mean(prec[Kmedias3$cluster==1]),col="red",pch=3,lwd=2)  # centroid 1st group
points(mean(hsol[Kmedias3$cluster==2]),mean(prec[Kmedias3$cluster==2]),col="blue",pch=3,lwd=2) # centroid 2nd group
points(mean(hsol[Kmedias3$cluster==3]),mean(prec[Kmedias3$cluster==3]),col="green",pch=3,lwd=2) # centroid 3rd group

# mapa:
map(regions=c("Spain","Portugal","Morocco","Algeria","France","Canary Islands"),xlim=c(-20,5),ylim=c(27,44))
points(lonxitude[Kmedias3$cluster==1],latitude[Kmedias3$cluster==1],cex=0.25,pch=1,col="red")
points(lonxitude[Kmedias3$cluster==2],latitude[Kmedias3$cluster==2],cex=0.25,pch=3,col="blue")
points(lonxitude[Kmedias3$cluster==3],latitude[Kmedias3$cluster==3],cex=0.25,pch=4,col="green")
text(lonxitude[Kmedias3$cluster==1],latitude[Kmedias3$cluster==1]+0.25,codigo[Kmedias3$cluster==1],cex=0.50,col="red")
text(lonxitude[Kmedias3$cluster==2],latitude[Kmedias3$cluster==2]+0.20,codigo[Kmedias3$cluster==2],cex=0.50,col="blue")
text(lonxitude[Kmedias3$cluster==3],latitude[Kmedias3$cluster==3]+0.20,codigo[Kmedias3$cluster==3],cex=0.50,col="green")


# gráfico da Figura 6.11
sswg <- numeric(5)
for (i in 1:5){
  sswg[i] <- kmeans(datos.para.cluster,centers=i)$tot.withinss
}

barplot(sswg[1:5],names.arg=1:5,xlab="K (número de clusters)",ylab="SSWG")



# 6.5.2. Métodos xerárquicos
############################

# Método do enlace completo
cx.completo <- hclust(dist(datos.para.cluster),method="complete")

# Dendograma:
plot(cx.completo,labels=cidade)
plot(cx.completo,labels=climacidades$cidade,cex=0.5,main="",xlab="",sub="")

# Cortar o dendograma a unha determinada altura
cutree(cx.completo,h=1500)

# Cortar o dendograma nun número de grupos
cutree(cx.completo,k=3)


# Método do enlace simple
cx.simple <- hclust(dist(datos.para.cluster),method="single")

# Método do centroide
cx.centroide <- hclust(dist(datos.para.cluster),method="centroid")


plot(cx.simple,labels=climacidades$cidade,cex=0.5,main="",xlab="",sub="")


detach(climacidades)




# 6.6. Análise discriminante
##########################################

diabetes <- read.table(file="datos-diabetes.txt",header=TRUE)
attach(diabetes)

# código para obter a Figura 6.15
# install.packages("lattice") 
library(lattice) 
splom(diabetes[,c("glu","bp","bmi")],groups=type,pch=c(1,3),col=c("blue","red"),cex=0.5)

# Regra discriminante de Fisher
# install.packages("MASS") 
library(MASS)
lda.diabetes <- lda(type~glu+bp+bmi)
lda.diabetes

# probabilidades a priori (p2 representa a prevalencia da diabetes)
p2 <- 0.10
p1 <- 1-p2

# punto de corte para o score
( c <- log(p1) - log(p2) )

# Predición:
muller.1 <- data.frame(110,50,21)
names(muller.1) <- c("glu","bp","bmi")
predict(lda.diabetes,muller.1,prior=c(p1,p2))


muller.2 <- data.frame(200,85,32)
names(muller.2) <- c("glu","bp","bmi")
predict(lda.diabetes,muller.2,prior=c(p1,p2))


# Táboa de confusión calculada sobre os mesmos datos (non recomendado)
clase.predita <- predict(lda.diabetes,diabetes[,c("glu","bp","bmi")],prior=c(p1,p2))$class
table(clase.predita,type)


# Táboa de confusión por validación cruzada (recomendado)
n <- length(type)
clase.predita <- factor(rep(NA,n),levels=c("No","Yes"))
for (i in 1:n){
  lda.vc <- lda(type~glu+bp+bmi,data=diabetes[-i,])
  clase.predita[i] <- 
    predict(lda.vc,diabetes[i,c("glu","bp","bmi")],prior=c(p1,p2))$class
}
table(clase.predita,type)
