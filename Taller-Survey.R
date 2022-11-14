## Librerias
library(survey)

######## Muestreo aleatorio simple ##########
sugar = read.table("sugar.txt", header=T)
N=nrow(sugar)  ; n=60  # tamaño muestral definido a priori
set.seed(12345); selec = sample(N, n)     # todos deben obtener la misma secuencia 
muestra = sugar[selec,]
# se agregan columnas con el inverso prob de selección y tamaño poblacional
muestra1 = cbind(muestra, PESOS=rep(N/n, n), TAM=rep(N,n))

head(muestra1)
dis1 = svydesign(id=~1, data=muestra1, weight=~PESOS, fpc=~TAM)
svymean(~cosecha, dis1)  # se indica la variable y el diseño muestral
svymean(~cosecha+valor+costo, dis1, deff=T) -> m1
m1; confint(m1, level=0.95)
svytotal(~cosecha+valor+costo, dis1, deff=T)

svytotal(~cosecha+area, dis1, deff=T)  # totales
# razón = suma cosecha / suma área

svyratio(~cosecha, ~area, dis1)
# resumen de los 5 números, con IdeC
svyquantile(~valor+costo, dis1, quantiles=c(0,25,50,75,100)/100, ci=T)
# Para proporciones – definir “factor” y utilizar svymean
Region_=factor(muestra1$region,)
svymean(~Region_, dis1)

svyby(~cosecha+valor, ~region, dis1, svymean)  # media y error estándar 
svyby(~cosecha+valor, ~region, dis1, svytotal)  # total y error estándar
# o descripción para segmentos. Por ejemplo, si área <60 vs > 60
summary(muestra1$area)
svyby(~cosecha+valor, ~I(muestra1$area < 60), dis1, svymean)
# alternativamente, con uso de subset
svymean(~cosecha+valor, subset(dis1, muestra1$area < 60))
# por último, si es para la region 1
svymean(~cosecha+valor, subset(dis1, region==1)) -> r1
r1 ; confint(r1)

############# Muestreo Estratificado ###############
table(sugar$region) ; table(muestra$region)
invprob=	table(sugar$region) /table(muestra$region)
Nh=table(sugar$region); nh=table(muestra$region)
# asignamos los pesos y tamaños de cada estrato
pesos = rep(invprob, nh) ; 	tam = rep(Nh, nh)
muestra=muestra[order(muestra$region),]   # hay que ordenar para asignar pesos
muestra2 = cbind(muestra, pesos, tam)
dis2 = svydesign(id=~1, strata=~region, data=muestra2, 
                 weight=~pesos, fpc=~tam)
svymean(~cosecha+valor, dis2, deff=T)
svyratio(~cosecha,~area, dis2, deff=T)
svyby(~cosecha,~region, dis2, svymean)

############# Conglomerados en UNA etapa ##################
data = read.table("ejclus.txt", header=T)  
head(data)  # incluye el inv probab selección y tamaño poblacional
dclus1 <- svydesign(ids=~congl, weight=~invprob, data=data, fpc=~N)
svymean(~yi, dclus1, deff=T)
svyratio(~yi,~mi, dclus1) -> a1
a1;confint(a1)
svytotal(~yi, dclus1) -> a2
a2;confint(a2)
svyratio(~ai, ~mi, dclus1) -> a3
a3;confint(a3)
