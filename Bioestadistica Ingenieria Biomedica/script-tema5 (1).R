##############################################################
#
# TEMA 5: REGRESIÓN
#
##############################################################


# 5.1. Gráfico de dispersión e coeficiente de correlación
##########################################################

saudegalicia2017 <- read.csv(file="datos-saudegalicia2017.csv")
attach(saudegalicia2017)

# Gráfico de dispersión
plot(altura,peso)
plot(altura,peso,frame=FALSE,xlab="altura",ylab="peso",cex=0.75,lwd=2)

# Coeficiente de correlación
cor(altura,peso)


# 5.2. Regresión lineal simple: a recta de regresión
####################################################


# 5.2.1. Estimación
###################

# estimación dos coeficientes da recta de regresión

# estimador do intercepto:
mean(peso) - cov(altura,peso)*mean(altura)/var(altura)

# estimador da pendente:
cov(altura,peso)/var(altura)

# función lm():
rr.pesoaltura <- lm(peso~altura)
rr.pesoaltura

abline(rr.pesoaltura,col="red")

summary(rr.pesoaltura)



# 5.2.3. Análise de residuos
############################

plot(rr.pesoaltura)



# 5.2.4. Tests de hipóteses en regresión
########################################

# Test de utilidade global: F-test
# Tests sobre os coeficientes: t-test

summary(rr.pesoaltura)


# 5.2.5. Predición: intervalos de confianza 
# para a media da resposta e intervalos de predición
#####################################################

# Predición do peso para unha persoa con  altura = 170 cm
puntos.predicion <- data.frame(altura=c(170))
predict(rr.pesoaltura,newdata=puntos.predicion)
predict(rr.pesoaltura,interval="confidence",newdata=puntos.predicion)
predict(rr.pesoaltura,interval="prediction",newdata=puntos.predicion)

#Código para obter o gráfico da Figura 5.7
puntos.predicion <- data.frame(altura=140:200)
ci <- predict(rr.pesoaltura,interval="confidence",newdata=puntos.predicion)
pi <- predict(rr.pesoaltura,interval="prediction",newdata=puntos.predicion)
plot(altura,peso,frame=FALSE,ylim=range(pi),cex=0.5,lwd=2)
matlines(puntos.predicion,ci,lty=c(1,2,2),lwd=c(1,2,2),col=c(1,2,2))
matlines(puntos.predicion,pi,lty=c(1,3,3),lwd=c(1,2,2),col=c(1,4,4))
    
detach(saudegalicia2017)
    

  
# 5.4. Regresión lineal múltiple
################################
  
# 5.4.1. O modelo de regresión lineal múltiple. Estimación
# 5.4.2. Tests en modelos de regresión múltiples 
#########################################################
  
calidadeaireNY <- read.table(file="datos-calidadeaireNY.txt",header=TRUE)
attach(calidadeaireNY)
  
# matriz de correlacións:
cor(calidadeaireNY)

# gráficos de dispersión por pares:
pairs(calidadeaireNY)

# Modelo lineal múltiple 
# (ver tests sobre o modelo completo e sobre os coeficientes)
modelo.Ozone <- lm(Ozone~Temp+Wind+Solar.R)
summary(modelo.Ozone)

# 5.4.3. Análise de residuos
############################

plot(modelo.Ozone)


# 5.4.4. Comparación de modelos xerárquicos
###########################################

modelo.restrinxido <- lm(Ozone~Temp)
modelo.completo <- lm(Ozone~Temp+Wind+Solar.R)

anova(modelo.restrinxido,modelo.completo)



# 5.6. Modelos de regresión avanzados
#####################################

# 5.6.1. Covariables nominais: codificación con variables dummy
###############################################################


diabetes <- read.table(file="datos-diabetes.txt",header=TRUE)
attach(diabetes)
plot(age,glu,type="n")
points(age[type=="Yes"],glu[type=="Yes"],col=2)
points(age[type=="No"],glu[type=="No"],col=4)

modelo.factor <- lm(glu ~ age + type)

model.matrix(modelo.factor)

summary(modelo.factor)

plot(modelo.factor)



# 5.6.2. Modelos con interaccións
#################################

# Interacción entre unha variable numérica e un factor

modelo.interaccion <- lm(bp~age+type+age*type)

summary(modelo.interaccion)


# Interacción entre dúas covariables numéricas

summary(lm(bmi~age+skin+age*skin))




# 5.6.3. Modelos non lineais: modelos polinómicos
#################################################

# Modelo polinómico de grao 2:
peixes <- read.table(file="datos-peixes.txt",header=TRUE)
summary(lm(Peso~Lonxitude+I(Lonxitude^2),data=peixes))

# Comparación de modelos polinómicos
modelo1 <- lm(Peso~Lonxitude,data=peixes)
modelo2 <- lm(Peso~Lonxitude+I(Lonxitude^2),data=peixes)
modelo3 <- lm(Peso~Lonxitude+I(Lonxitude^2)+I(Lonxitude^3),data=peixes)
anova(modelo1,modelo2,modelo3)




# 5.7. Regresión con resposta cualitativa: regresión loxística
##############################################################

modelo.loxistico <- glm(factor(type)~glu,family=binomial,data=diabetes)
summary(modelo.loxistico)

# Predición
valor.glu <- data.frame(glu=c(180))
predict(modelo.loxistico,valor.glu,type="response")

# Código para obter o gráfico de p(x) (Figura 5.14)

valores.glu <- data.frame(glu=seq(min(glu),max(glu),by=1))
plot(valores.glu$glu,
     predict(modelo.loxistico,valores.glu,type="response"),type="l",xlab="glu",ylab="p(x)")





