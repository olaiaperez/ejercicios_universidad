

######################################
#
#    Grao en Enxeñería Biomédica
#
#         BIOESTATÍSTICA
#
#
#   Juan Carlos Pardo-Fernandez
#      Universidade de Vigo
#
#
######################################


##########################
# CUESTIÓNS BÁSICAS DE R
##########################

# Web de R: https://www.r-project.org
# Web de RStudio: https://rstudio.com/

# axuda

help.start() # axuda xeral
help(mean)   # axuda sobre unha función
?mean        # axuda sobre unha función


# R como calculadora

5+4/2-3*2
5^2


# funcións matemáticas 

sqrt(16)
log(2)  # logaritmo natural
log10(1000) # logaritmo en base 10
log(32,base=2) 
exp(1)
sin(pi/2)
cos(pi)


# obxectos e símbolo de asignación

x<-5 # o símbolo de asignación é "<-" ou "="
x=5  # pódense empregar os dous indistintamente
y<-7
x+y
x<-50
x+y

X+y  # atención: R distingue maiúsculas e minúsculas!


# vectores

u <- c(7,5,6,8,13,9,10)
v <- c(1,1,2,2,2,3,3)
u[2] # extrae un elemento dun vector
u[c(2,4,5)] # extrae varios elementos dun vector
u[-c(2,6)] # extrae todos os elementos, agás os indicados


# operadores sobre vectores

sum(u)
mean(u)
prod(u)
max(u)
min(u)
length(u)


# ordenar

sort(u)      # ordena os elementos dun vector en orde crecente
order(u)     # atopa as posicións para ordenar un vector en orde crecente
u[order(u)]  # fai o mesmo ca sort(u)
v[order(u)]  # reordena o vector v de acordo con order(u)


# operacións entre vectores elemento a elemento

u+v     # suma
u*v     # produto
u^2     # cadrado
sqrt(u) # raíz cadrada
log(u)  # función matemática


# operacións entre vectores e constantes

k<-7
u+k 
u-k 
k*u
u/k


# como crear vectores:

rep(1,10)
rep(c(1,2),5)
rep(c(1,2),each=5)

1:10
7:15

seq(0,10,by=0.5)
seq(0,10,length=41)


# operadores binarios e lóxicos

u>5  # comparcións numéricas: ==, <, <=, >, >=
u<=9
u==7  # atención ao símbolo "==" (é doado esquecerse e poñer só un)
(u>7) & (u<9)  # operador lóxico "E"
(u<7) | (u>9)  # operador lóxico "OU"
!(u>9)  		   # operador lóxico "NON"


# matrices

M1 <- matrix(c(1,2,3,4,5,6,7,8,9,10),ncol=2,byrow=TRUE)
M2 <- matrix(c(1,2,3,4,5,6),nrow=2,byrow=FALSE)

dim(M1)        # dimensións dunha matrix
M1[,1]         # extrae a segunda columna
M1[c(2,4,5),]  # extrae as filas 2, 4 e 5
M1[1:3,2]
sum(M1)
colSums(M1)
rowSums(M1)

M1%*%M2   # produto matricial


# crear matrices a partir de vectores:

cbind(u,v)    # combina os vectores u e v como columnas
rbind(u,v)	  # combina os vectores u e v como filas


# espazo de traballo

objects()     # lista os obxectos no espazo de traballo
ls()          # lista os obxectos no espazo de traballo
rm(x)         # elimina o obxecto x do espazo de traballo 
rm(list=ls()) # elimina todos os obxectos do espazo de traballo


# IMPORTANTE! directorio de traballo

getwd()   # indica o "path" do directorio de traballo da sesión actual
setwd()   # permite cambiar do directorio de traballo. 
# NOTA: En RStudio tamén se pode cambiar o directorio de traballo 
# a través do menú "Session > Set Working Directory"

 
##############################################################
#
# TEMA 1: REVISIÓN DE TÉCNICAS DESCRITIVAS E SOFTWARE R
#
##############################################################


# 1.1.5 Importar conxuntos de datos en R
###################################################

diabetes <- read.table("datos-diabetes.txt",header=TRUE)

# diabetes <- read.csv("datos-diabetes.csv",head=TRUE,dec=".",sep=",")



# información xeral do conxunto de datos ("data frame")

class(diabetes)

str(diabetes)

head(diabetes)  # por defecto amosa as 6 primeiras liñas

head(diabetes,10)  # amosa as 10 primeiras liñas


# como acceder a unha variable dun conxunto de datos 

diabetes$glu
diabetes[,1]
diabetes[,"glu"]


# attach e detach

attach(diabetes)  # incorpora as variables ao espazo de traballo
# detach(diabetes) # elimina as variables do espazo de traballo pero mantén o conxunto de datos





# 1.2.1 Táboas de frecuencias
###################################################

attach(diabetes)

table(type)

n <- length(type)

table(type)/n

100*table(type)/n

table(age)

table(cut(age,breaks=seq(20,65,by=5)))

100*table(cut(age,breaks=seq(20,65,by=5)))/n

table(cut(glu,breaks=c(50,75,100,125,150,175,200)))

100*cumsum(table(cut(glu,breaks=c(50,75,100,125,150,175,200))))/n





# 1.2.2 Gráficos
###################################################

# gráfico de sectores

pie(table(type))

# gráficos de barras

barplot(table(type))


# gráficos de barras combinando dúas variables
# (datos RATPUPS)

ratpups <- read.table("datos-ratpups.txt",header=TRUE)

barplot(table(ratpups$sex,ratpups$treatment),beside=TRUE,legend=TRUE)

# establecer a orde dos niveis dun factor:

ratpups$treatment <- factor(ratpups$treatment,levels=c("control","low","high"))

barplot(table(ratpups$sex,ratpups$treatment),beside=TRUE,legend=TRUE)

barplot(table(ratpups$sex,ratpups$treatment),beside=FALSE,legend=TRUE)


# histograma

hist(glu[type=="No"])
hist(glu[type=="Yes"])

# os dous histogramas no mesmo gráfico representados
# na mesma escala e cambiando algúns parámetros gráficos

par(mfrow=c(1,2))  # disposición do papel gráfico (1 fila e 2 columnas)
hist(glu[type=="No"],freq=FALSE,main=" ",xlab="concentración de glucosa",ylab="densidade",xlim=c(50,200))
hist(glu[type=="Yes"],freq=FALSE,main=" ",xlab="concentración de glucosa",ylab="densidade",xlim=c(50,200))
par(mfrow=c(1,1)) # volver a un único gráfico


# 1.2.3 Medidas resumo
###################################################

mean(age)

median(age)

quantile(age,c(0.10,0.25,0.50,0.75,0.90))

summary(age)

summary(age[type=="No"])

summary(age[type=="Yes"])

tapply(age,type,summary)

boxplot(glu~type)

boxplot(glu~type,frame=FALSE,ylab="concentración de glucosa")


