Base<- Christopher_I3_
install.packages("xlsx")
library(xlsx)
Base<-data.frame(Base)

#Cambiaremos los nombres ya que probando tuve problemas al identificarlos 
colnames(Base)[2] <- "ZONA"
colnames(Base)[3] <-"Sexo"
colnames(Base)[4] <- "Edad"
colnames(Base)[7] <- "GSE"
colnames(Base)[9] <- "pesos"

# Cargaremos los GSE del enunciado 
GSE <- c(0.086, 0.188, 0.263, 0.463)


#Cargaremos como el codigo visto enclases la baase INE

INE <- read_excel("C:/Users/clweb/Downloads/INE-estimaciones-y-proyecciones-2002-2035-comunas (1).xlsx",sheet="Est. y Proy. de Pob. Comunal")
#Cambiaremos 2 Nombres por que debo mencionar tuve problemas para hacer correr el codigo


colnames(INE)[31] <- "pobla2021"
colnames(INE)[10] <- "Sexo"
colnames(INE)[9] <- "Edad"
#Lo pasamos a aun datafrma
INE <- as.data.frame(INE)




#Usamos lo visto en clases y esclarecemos los parametros vistos en clases
target <- with(INE, list(
  Sexo = wpct(Sexo,pobla2021),
  Edad = wpct(Edad, pobla2021),
  ZONA = wpct(ZONA, pobla2021), GSE=GSE
))
names(target)<-c("Sexo","Edad", "ZONA", "GSE")
names(target$GSE) <- c(1,2,3,4)

anesrakefinder(target, Base, choosemethod = "total", weightvec = Base$pesos10)
unique(Base$GSE)
outsave <- anesrake(target, Base, caseid = Base$num, weightvec=Base$pesos10,
                    verbose= FALSE, cap = 5, choosemethod = "total",
                    type = "pctlim", pctlim = .05 , nlim = 5,
                    iterate = TRUE , force1 = TRUE)

Base$pesos.actulizados <- as.numeric(outsave$weightvec)

write.xlsx(Base, "Ultimosuspiro.xlsx")

# Ahora de esta base copiaremos la columa de pesos actualziados y la pasaremos al a que en 
#enviaremos 
