
datos <- rio::import("Encuesta.xlsx")
library(survey)
library(tidyverse)

# a) --------------------------------------------------------------

#Primero se eligen las manzanas
#Segundo se elige la vivienda
#Al final se elige la persona

datos$zona <- as.factor(datos$zona)

aux <- matrix(table(datos$zona, datos$IdMz), nrow = 4)

manzanas.zona <- rowSums(ifelse(aux!=0, 1, 0))

## Probabilidades de selección de la vivienda

datos <- datos %>% 
  mutate(mxzona = manzanas.zona[zona]) %>% 
  mutate(p.sel1 =  (1/mxzona) * (1/Nviv), #para la etapa 1 (vivienda)
         p.sel2 = (1/Nelig)) #para la etapa 2 (persona)

# b) --------------------------------------------------------------


