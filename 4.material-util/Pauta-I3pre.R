############### 02 / DIC ###################
## Librerias
library(rio)
library(dplyr)
library(readxl)
library(anesrake)
library(weights)
library(expss)
library(questionr)
library(tidyverse)


########################## Int3  MUESTREO
base <-rio::import(file.choose())
head(base)

######################## a) 0.5 puntos
# tabla de frecuencias 
# a) Tabla de % - respecto al total -de la muestra y de la poblaci?n
print("a) distribuci?n de la muestra obtenida segun Zona, Sexo-Cat.edad") 
cro_cases(base$zona,(base$sexo %nest% base$cat.edad))
## las etiquetas se DEBEN poner aqu? o bien en el informe...pero tienen que ir!

print("a1) distribuci?n % de la muestra por Zona para Sexo-Cat.edad") 
cro_rpct(list(base$zona, total()),(base$sexo %nest% base$cat.edad))

pobl <- readxl::read_excel("marcoReferencia.xlsx", sheet="Marco2")  # archivo creado a partir de la proy censo
print("a2) distribuci?n % de la poblacion por Zona para Sexo-Cat.edad")
cro_rpct(list(pobl$zona, total()), (pobl$Sexo %nest% pobl$Cat), weight=pobl$Pob2021)

######################## b) 0.5 puntos
# b) redistribucion del voto

cro_cpct(base$P2v,list(base$P1v ,total()))
# notese la distribucion de votos de Parisi entre Boric y Kast

######################## c) 1.0 punto
# c) construccion de tablas para obtener g-weights: ZOSEED
base<-unite(base, ZOSEED,c("zona","sexo","cat.edad"),  sep = "", remove = FALSE)
table(base$ZOSEED)/sum(table(base$ZOSEED))-> PMuestra
PPobl <- readxl::read_excel("marcoReferencia.xlsx", sheet="Marco3")  # archivo creado a partir de la proy censo
cc <- cbind(PMuestra, PPobl)
cc$POND1 <- cc$PESOPOB / cc$Freq
ponderador <- data.frame(cc$ZOSEED, cc$POND1)
names(ponderador)<-c("ZOSEED","POND1")
base1 <- merge(base, ponderador)
cro_rpct(list(base1$zona, total()),(base1$sexo %nest% base1$cat.edad), weight=base1$POND1)
base1$FEXP1 = 15200840*base1$POND1/sum(base1$POND1)

# c) redistribucion del voto - con ponderacion g-weights
cro_cpct(base1$P2v,list(base1$P1v ,total()), weight=base1$POND1)

######################## d) 1.0 punto

base1$Sexo=cut(base1$sexo, breaks=c(0,1,2),
    labels = c("Hombre","Mujer"))
base1$GSE=cut(base1$gse, breaks=c(0,1,2,3,6),
    labels = c("C1", "C2", "C3", "DE"))
base1$Zona = cut(base1$zona, breaks=c(0,1,2,3,4),
    labels = c("1","2","3","4"))
base1$CAND <- cut(base1$P1v, breaks=c(0,1,2,7,10),
    labels = c("B", "K", "O", "NO"))
table(base1$P1v,base1$CAND)
table(base1$P1v)

# valores provisto - tablas
GSE <- c(0.086, 0.188, 0.263, 0.463)
CAND <- c(0.53,0.121,0.131,0.218)

pop <- readxl::read_excel("marcoReferencia.xlsx", sheet="Marco2")
######### por Zona, Sexo y Cat.Edad ...
target <- with(pop, list(
  Sexo  = wpct(Sexo,Pob2021),
  Categ = wpct(Cat, Pob2021),
  Zonan = wpct(zona, Pob2021), GSE=GSE))
names(target)<-c("Sexo","cat.edad", "Zona", "GSE")
head(base1)

#names(target$CAND) <- levels(base1$CAND)
names(target$GSE) <- levels(base1$GSE)
names(target$Sexo) <- levels(base1$Sexo)

base1$caso<-1:length(base1$sexo)


anesrakefinder(target, base1, choosemethod = "total", weightvec=base1$POND1)
outsave <- anesrake(target, base1, caseid = base1$caso, weightvec=base1$POND1,
  verbose= FALSE, cap = 5, choosemethod = "total",
  type = "pctlim", pctlim = .05 , nlim = 5,
  iterate = TRUE , force1 = TRUE)

summary(outsave)

# se guardan los ponderadores..
base1$POND2  <- unlist(outsave[1])

cro_cpct(base1$P2v,list(base1$P1v ,total()), weight=base1$POND2)


######################## e) 1.0 punto

# valores provisto - tablas
GSE <- c(0.086, 0.188, 0.263, 0.463)
CAND <- c(0.53,0.121,0.131,0.218)

pop <- readxl::read_excel("marcoReferencia.xlsx", sheet="Marco2")
######### por Zona, Sexo y Cat.Edad ...
target <- with(pop, list(
  Sexo  = wpct(Sexo,Pob2021),
  Categ = wpct(Cat, Pob2021),
  Zonan = wpct(zona, Pob2021),  CAND=CAND))
names(target)<-c("Sexo","cat.edad", "Zona","CAND")
head(base1)

names(target$CAND) <- levels(base1$CAND)
#names(target$GSE) <- levels(base1$GSE)
names(target$Sexo) <- levels(base1$Sexo)

base1$caso<-1:length(base1$sexo)


anesrakefinder(target, base1, choosemethod = "total", weightvec=base1$POND2)
outsave <- anesrake(target, base1, caseid = base1$caso, weightvec=base1$POND2,
  verbose= FALSE, cap = 5, choosemethod = "total",
  type = "pctlim", pctlim = .01 , nlim = 5,
  iterate = TRUE , force1 = TRUE)

summary(outsave)

# se guardan los ponderadores..
base1$POND3  <- unlist(outsave[1])

cro_cpct(base1$P2v,list(base1$P1v ,total()), weight=base1$POND3)
head(base1)

#################### LOS QUE TRABAJARON SOLOS .. HASTA AQUI #########

######################## f) 0.5 puntos


######################## g) 1.5 puntos

print("distribuci?n % de la muestra por Zona para Sexo-Cat.edad - con POND3")
cro_rpct(list(base1$Zona, total()), (base1$Sexo %nest% base1$cat.edad), weight=base1$POND3)
head(base1)
print("distribuci?n % de la poblacion por Zona para Sexo-Cat.edad")
cro_rpct(list(pobl$zona, total()), (pobl$Sexo %nest% pobl$Cat), weight=pobl$Pob2021)
