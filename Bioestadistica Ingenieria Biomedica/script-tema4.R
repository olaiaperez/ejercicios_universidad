##############################################################
#
# TEMA 4: TÁBOAS DE CONTINXENCIA
#
##############################################################


# 4.1. INTRODUCIÓN
###################################################

# Datos extraídos da Enquisa Nacional de Saúde 2017 (INE)

saudegalicia2017 <- read.csv(file="datos-saudegalicia2017.csv")

attach(saudegalicia2017)

# Táboa de continxencia sexo-estado.saude
table(sexo,estado.saude)

# Táboa de frecuencias da variable sexo
# (totais de fila da táboa de continxencia)
table(sexo)
rowSums(table(sexo,estado.saude))

# Táboa de frecuencias da variable estado.saude
# (totais de columna da táboa de continxencia)
table(estado.saude)
colSums(table(sexo,estado.saude))



# 4.2. DISTRIBUCIÓN CONXUNTA, MARXINAL E CONDICIONADA
######################################################

# distribución conxunta sexo-estado.saude
taboa.continxencia <- table(sexo,estado.saude)
n <- sum(taboa.continxencia)
distrib.conxunta <- taboa.continxencia/n
distrib.conxunta

# distribución marxinal da variable sexo
distrib.marxinal.sexo <- table(sexo)/n
distrib.marxinal.sexo

# tamén podemos facer
rowSums(distrib.conxunta)

# distribución marxinal da variable estado.saude
distrib.marxinal.estado.saude <- table(estado.saude)/n
distrib.marxinal.estado.saude

# tamén podemos facer
colSums(distrib.conxunta)


# Distribución condicionada da variable estado.saude
# para sexo="home"
taboa.continxencia[1,]/sum(sexo=="H")



# 4.3. O GRÁFICO DE MOSAICO
###########################    


#  A función mosaic() pertence ao paquete "vcd"

#install.packages("vcd")
library(vcd)
mosaic(table(sexo,estado.saude))
mosaic(table(artrose,estado.saude))
qchisq(0.95,df=4) #me da el valor critico
1-pchisq(4.12,df=4) #pvalor

#si las variables son independientes entonces se declararian con igual estado de salud
#no guardan motivos para justificar que uno tenga mejor estado de salud que el otro, no hay relación
#es muy importante 


# 4.4. O TEST DA CHI-CADRADO DE INDEPENDENCIA
#############################################

#  H_0: As variables "sexo" e "estado.saude" son independentes
#  H_1: As variables "sexo" e "estado.saude" non son independentes

chisq.test(table(sexo,estado.saude)) #es un test asintotica, aproximada, por eso da error


# A táboa de frecuencias esperadas tamén se 
# obtén a partir da función chisq.test():
chisq.test(table(sexo,estado.saude))$expected
##el test puede funcionar si tengo frecuencias esperadas muy pequeñas



#  H_0: As variables "artrose" e "estado.saude" son independentes
#  H_1: As variables "artrose" e "estado.saude" non son independentes

chisq.test(table(artrose,estado.saude))
chisq.test(table(artrose,estado.saude))$expected
#si en el grafico aparece una barra significa que en ese grupo no hay muestras
#moda del grupo no artrosis es 2 y del grupo artrosis es 3, mejor estado de salud el no artrosis

# Colapsar as columnas 4 e 5 nunha única columna, simplemente es una codificacion
estado.saude2 <- estado.saude*(estado.saude==1 | estado.saude==2 | estado.saude==3) + 4*(estado.saude== 4 | estado.saude==5)
chisq.test(table(artrose,estado.saude2))$expected
chisq.test(table(artrose,estado.saude2))



# 4.5. TÁBOAS 2 X 2: PROPORCIÓNS, RISCOS RELATIVOS E ODDS RATIOS
################################################################
#cuando se comparan dos variables binarias, a las mujeres les afecta más la artrosis

table(sexo,artrose)


