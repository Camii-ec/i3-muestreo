################# Muestreo ##############
# Primera parte
# 1. Tama?o de muestra
# Media
sample.size.mean(e=3, S=30, N=3632)
## 348
# Proporcion
sample.size.prop(e=0.02, P=0.06, N=3632) 
## 542
# Resp: max{248, 542} = 542
n = 542

############################################
# 2. Seleccion de la muestra - BaseRBD.xlsx
RBD <- rio::import("BaseRBD.xlsx")
head(RBD)
table(RBD$Region)
N = nrow(RBD)
set.seed(2109)
muestra = sample(N, n, replace=F)
muestraRBD = RBD[muestra,]
table(muestraRBD$Region)

###########################################
#3. Recoleccion de la informacion muestral
Datos <- rio::import("BaseEstablecimientos.xlsx")
head(Datos)
base <- merge(Datos, muestraRBD)
head(base)

##############################################
#4. Tabulacion - uso comandos Smean y Sprop
Smean(base$MAT, N=N)
Sprop(base$MAT<200, N=N)
# permite "rellenar" parte de la 1era l?nea
# para los Totales, sabemos N*media, N*Error - variable: Nalum
N*Smean(base$Nalum, N=N)$mean  # num total de alumnos
N*Smean(base$Nalum, N=N)$se    # error est?ndar asociado

# el caso de alumnos en riesgo, la estimaci?n nos indica que 
# solo una proporcion de establecimientos esta en riesgo.
# asi, que primeramente obtenemos el tama?o medio de los estabRiesgo
# y aplicamos "la proporci?n".
################################################
riesgo = base[base$MAT<200,]
Smean(riesgo$Nalum, N=N)
Smean(riesgo$Nalum, N=N)$mean*N*Sprop(base$MAT<200, N=N)$p
Smean(riesgo$Nalum, N=N)$se*N*Sprop(base$MAT<200, N=N)$p

################################################
# estimaciones regionales 
aggregate(base$MAT~base$Region, FUN=mean)
aggregate(base$MAT~base$Region, FUN=sd)
table(base$Region)  # tama?os de muestra, los n_i

# obtenemos errores est?ndar de las estimaciones sd_i/ra?z(n_i)
casos <- c(112, 145, 283)
aggregate(base$MAT~base$Region,, FUN=sd)-> est
est[,2]/sqrt(casos)

# para el total de alumnos - reiteramos, pero con Nalum
Nreg = c(766, 1058, 1808)     # totales poblacionales regionales
aggregate(base$Nalum~base$Region, FUN=mean) -> meaR
aggregate(base$Nalum~base$Region,, FUN=sd) -> estR
meaR[,2]*Nreg
Nreg*estR[,2]/sqrt(casos)

# Ahora, proporcion de establecimientos en riesgo por region:
base$riesgo = ifelse(base$MAT<200, 1, 0)
aggregate(base$riesgo~base$Region, FUN=mean)
aggregate(base$riesgo~base$Region, FUN=sd)
aggregate(base$riesgo~base$Region, FUN=sd)-> sdR
sdR[,2]/sqrt(casos)

###################################################
###################################################
# Segunda Parte
# 1. Tama?o de muestra
# muestra estratificada y proporcional
# para la media, con error = 3 e informacion sobre Sh y Nh
Sh = c(35, 30, 25)
Nh = c(1058, 1808, 766)
stratasize(e=3, Sh=Sh, Nh=Nh, type="prop")
## 361
# para la proporcion con error = 2% e informacion sobre Ph y Nh
PQ = c(.1*.9, .038*.962,.054*.946)
Sh = sqrt(PQ)
# t?cnicamente, lo enga?amos "pidiendo" media, cuando en realidad es
# una proporci?n... pero una proporci?n es una "media" de 0 y 1.
stratasize(e=0.02, Sh=Sh, Nh=Nh, type="prop")
## 463
# Tama?o de muestra = max(361, 463) = 463
# distribuimos, con afijaci?n proporcional
# usando "proporciones"
stratasamp(n=463, Sh=Sh, Nh=Nh)
# usando "desviaciones".. 
Sh = c(35, 30, 25)
stratasamp(n=463, Sh=Sh, Nh=Nh)
# resultado coincidente, dado que solo mira los Nh

# truco para seleccionar la muestra:
set.seed(2109)
N1=1058 ; n1=135
m1 = sample(N1, n1, replace=F)
muestra1 = RBD[RBD$Region=="DEL BIOB?O",][m1,]
N2=1808 ; n2=230
m2 = sample(N2, n2, replace=F)
muestra2 = RBD[RBD$Region=="METROPOLITANA DE SANTIAGO",][m2,]
N3=766 ; n3=98
m3 = sample(N3, n3, replace=F)
muestra3 = RBD[RBD$Region=="DE VALPARA?SO",][m3,]
muestraR = rbind(muestra1, muestra2, muestra3)
table(muestraR$Region)

#Recoleccion de la informacion: 
Datos$riesgo = ifelse(Datos$MAT<200, 1,0)  # variable 0/1 -> riesgo
baseE = merge(Datos, muestraR)
head(baseE)
nh = table(baseE$Region) 

rbind(cbind(Nh, nh),cbind(sum(Nh),sum(nh)))
stratamean(baseE$MAT, baseE$Region, Nh, eae=TRUE)
stratamean(baseE$riesgo, baseE$Region, Nh, eae=TRUE)

# para el numero total de alumnos
stratamean(baseE$Nalum, baseE$Region, Nh, eae=TRUE)

# basta con multiplicar por Nh - mean y error est?ndar



