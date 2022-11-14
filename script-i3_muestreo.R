
datos <- rio::import("Encuesta.xlsx")
library(survey)

# a) --------------------------------------------------------------

N <- 1000



# Cruce Primera vuelta v/s Segunda Vuelta

sort(table(datos$P1v))
sort(table(datos$P2v))
cruce_1v_2v <- table(datos$P1v,datos$P2v)
round(prop.table(cruce_1v_2v,1)*100,3)

# Pregunta 2 --------------------------------------------------------------


