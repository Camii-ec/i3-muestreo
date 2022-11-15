#### Trabajo Final - Muestreo
## Ricardo Parra

dir("Trabajo final/Datos")

library(readr)
library(dplyr)
library(anesrake)
library(openxlsx)
library(readxl)
library(tidyverse)


Base1 <- read_excel("Trabajo final/Datos/Trabajo_final_Ricardo_Parra1.xlsx")
Base1$num = 1:1443
skimr::skim(Base1)
datos = Base1 %>% select(elec_pres_1,elec_pres_2,
                         zona_u_r,Zona,sexo,
                         Región,Región_num,
                         edad,cedad,gse,num)
class(datos)
datos = as.data.frame(datos)
datos = datos %>% mutate(Sexos = str_sub(sexo,6,-1))
datos = datos %>% mutate(GSE_f = str_sub(gse,6,-1))



datos$cedad = as.factor(datos$cedad)
datos$zona_u_r = as.factor(datos$zona_u_r)
datos$Zona = as.factor(datos$Zona)
datos$Sexos = as.factor(datos$Sexos)
datos$Región = as.factor(datos$Región)
datos$GSE_f = as.factor(datos$GSE_f)


# Definimos los vectores para el target -----------------------------------

sexo = c("HOMBRE" = 0.493359737,
         "MUJER" = 0.506640263)

Gse = c("ABC1" = 0.141,
        "C2" = 0.112,
        "C3" = 0.247,
        "D" = 0.36,
        "E" = 0.14)

Region = c("Región 1" = 0.019877924,
           "Región 2" = 0.035762426,
           "Región 3" = 0.016095699,
           "Región 4" = 0.04309703,
           "Región 5" = 0.100586263,
           "Región 6" = 0.050865969,
           "Región 7" = 0.05808471,
           "Región 8" = 0.084894765,
           "Región 9" = 0.05181061,
           "Región 10" = 0.045598458,
           "Región 11" = 0.005474896,
           "Región 12" = 0.009144511,
           "Región 13" = 0.418858977,
           "Región 14" = 0.020724183,
           "Región 15" = 0.012977706,
           "Región 16" = 0.026145874)*100

sum(Region)

area = c("URBANA" = 0.885780692,
         "RURAL" = 0.114219308)

unique(datos$zona_u_r)
Cedad = c("1" = 0.23399674,
          "2" = 0.294901795,
          "3" = 0.242677049,
          "4" = 0.228424416)

Zona = c("Norte" = 0.127810784,
         "Centro" = 0.320577581,
         "Sur" = 0.132752658,
         "RM" = 0.418858977)

# Pregunta 1 --------------------------------------------------------------

target1 = list(zona_u_r = area, Región = Region,
               Sexos = sexo)

outsave1 = anesrake(target1,datos,caseid = datos$num)





# Pregunta 2 --------------------------------------------------------------

target2 = list(zona_u_r = area, Región = Region,
               cedad = Cedad)
outsave2 = anesrake(target2,datos,caseid = datos$num)


# Pregunta 3 --------------------------------------------------------------

target3 = list(Región = Region,GSE_f = Gse)

outsave3 = anesrake(target3,datos,caseid = datos$num)

# Pregunta 4 --------------------------------------------------------------

target4 = list(Zona = Zona ,zona_u_r = area,
               Sexos = sexo,cedad = Cedad)

outsave4 = anesrake(target4,datos,caseid = datos$num)

# Pregunta 5 --------------------------------------------------------------

target5 = list(Zona = Zona, zona_u_r = area,
               Sexos = sexo, cedad = Cedad,
               GSE_f = Gse)
outsave5 = anesrake(target5,datos,caseid = datos$num)


Ponderadores = data.frame("Pond1" = outsave1$weightvec,
                          "Pond2" = outsave2$weightvec,
                          "Pond3" = outsave3$weightvec,
                          "Pond4" = outsave4$weightvec,
                          "Pond5" = outsave5$weightvec)


##write.xlsx(Ponderadores,"Trabajo final/ponderadores.xlsx")



