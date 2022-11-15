
# Interrogacion 3 - EYP2417 -----------------------------------------------

## Cristian Dibán - Ricardo Parra

library(rio)
library(dplyr)
library(skimr)
library(anesrake)
library(openxlsx)

dir("Pruebas/I3")

data = import("Pruebas/I3/I3_EYP2417_CDiban_RParra.xlsx",sheet = 1)

skim(data)

datos = data[,1:11]
a = which(datos$gse == 5)
datos$gse[a]= 4


unique(datos$gse)

Sexo = c("Hombre"= 0.48873161,"Mujer"= 0.51126839)
Categ = c("18-29"= 0.23399674,
          "30-44"= 0.294901795,
          "45-59"= 0.242677049,
          "60 y+"= 0.228424416)

Zona = c("Norte"= 0.124041895,
         "Centro"= 0.321167383,
         "Sur"= 0.131785151,
         "RM"= 0.423005571)
GSE = c(0.086, 0.188, 0.263, 0.463)
sum(Categ)

target = list(zona = Zona,sexo = Sexo,cat.edad=Categ,
              gse = GSE)
outsave = anesrake(target, datos, caseid = datos$num,weightvec = datos$Pond...10)
head(outsave$weightvec)
data[,12] = outsave$weightvec

##write.xlsx(data,"i3_nuevo.xlsx")

Voto = c("Boric"= 0.121,"Kast" = 0.131,
         "No voto" = 0.530,"Otros" = 0.218)
target2 = list(zona = Zona,sexo = Sexo,cat.edad = Categ,
              P2v.2 = Voto)

data2 = import("Pruebas/I3/I3_EYP2417_CDiban_RParra2.xlsx",sheet = 2)


datos2 = data2[,1:13]
a2 = which(datos2$P2v == 10)
datos2$P2v[a2]= 8

datos2 = datos2 %>% mutate(P2v.2 = case_when(P2v == 1 ~ 1,
                                             P2v == 2 ~ 2,
                                             P2v == 8 ~ 3,
                                             P2v == 9 ~ 4))

unique(datos2$P2v.2)

outsave2 = anesrake(target2, datos2, caseid = datos2$num,weightvec = datos2$Pond...10)
head(outsave2$weightvec)
data2[,12] = outsave$weightvec
data2[,14] = outsave2$weightvec

##write.xlsx(data2,"i3_nuevo2.xlsx")

data3 = import("Pruebas/I3/I3_EYP2417_CDiban_RParra2.xlsx",sheet = 2)

datos3 = data3[,1:15]
a3 = which(datos3$gse == 5)
datos3$gse[a3]= 4

target3 = list(zona = Zona,sexo = Sexo,cat.edad = Categ,
               gse = GSE)

outsave3 = anesrake(target3, datos3, caseid = datos2$num,weightvec = datos3$Pond)
head(outsave3$weightvec)
data3[,16]=outsave3$weightvec

##write.xlsx(data3,"i3_nuevo3.xlsx")

