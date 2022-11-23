install.packages("MVA")  #1
library("MVA")#2
x11()#3
demo("Ch-MVA")#4
install.packages("mvtnorm")
hypo
class("hypo")
class(hypo)
hypo[1:5,"health"]
hypo[,c("sex","age")]
x=hypo[,c("sex","age")]
x
hypo["IQ"]
class(hypo["IQ"])
#Tarea: como trabajar con los NA y con ejemplo.
#CONJUNTO DE DATOS MULTIVARIADOS
measure #5
#pregunta 1: como hacer que todas las medidas se hagan una: Combinaciones lineales
#(componentes principales)
#Y1= C1X1+C2X2+C3X3
#hay subtipos de formas corporales entre los hombres y mujeres dentro de las cuales
#los individuos tienen formas similares y entre las cualeslas formas corporales difieran
#para responder necesitamos el analisis cluster
pottery #6
#los perfiles quimicos sugieren diferentes tipos de ollas y  si alguno de estos tipos de ollas y si alguno de estos tipos esta
#relacionado con el horno de la region?
exam #7
#los puntajes de los examenes reflejan algun rasgo subycente en un estudiante que no se puede medir directamente
USairpollution #7
measure[,c()]
x=measure[,c("chest","waist","hips")]
x
cov(x)
var(x)
xfemale=subset(measure,gender=="female")
xfemale
xfemale=subset(measure,gender=="female")[,c("chest","waist","hips")]
xfemale1
cov(xfemale1)
xmale=subset(measure,gender=="male")[,c("chest","waist","hips")]
xmale
cov(xmale)
cor(xfemale1)
cor(x)
cor(xmale)
scale(x,center=FALSE)#aqui se estandariza
dist(scale(x,center=FALSE))

# Clase 27 septiembre

# Diagrama de dispersion

mlab<- "Empresas manufactureras con 20 empleados o mas"
plab<- "Tamaño de la pobalcion en miles (censo 1970)"
plot(popul ~ manu, data = USairpollution, xlab = mlab, ylab = plab)
# Diagrama de caja bivariado
rug(USairpollution$manu, side = 1)
rug(USairpollution$popul, side = 2)
hist(USairpollution$manu, main = "")
boxplot(USairpollution$popul)
layout(matrix(c(2, 0, 1, 3), nrow = 2, byrow = TRUE),widths = c(2, 1), heights = c(1, 2), respect = TRUE)
# xlim<-range(USairpollution$manu)*1.1
xlim<-with(USairpollution,range(manu))*1.1
plot(popul ~ manu, data = USairpollution, cex.lab=0.9, xlab = mlab, ylab = plab, type="n",xlim=xlim)
with(USairpollution, text(manu, popul, cex=0.5, labels=abbreviate(row.names(USairpollution))))

# Clase 03 de octubre

hist(USairpollution$manu, main = "")
boxplot(USairpollution$popul)
# diagrama de caja bivariado
lab<-c("Chicago","Detroit","Cleveland","Philadelphia","Houston")
outcity<-match(lab, rownames(USairpollution))
x<-USairpollution[,c("manu","popul")]
x
bvbox(x,mtitle = "",xlab = mlab,ylab = plab)
text(x$manu[outcity],x$popul[outcity],labels=lab,cex=0.7,pos=c(2,2,4,2,2))
# la elippse central concentra al menos el 50% de los datos y se llama HINGE (bisagra), la elipse de afuera se llama FENCE (valla). No utiliza los valores atìpicos
# Correlaciòn
with(USairpollution, cor(manu, popul))
outcity1<-match(c("Chicago","Philadelphia","Detroit","Cleveland"), rownames(USairpollution))
outcity1
with(USairpollution, cor(manu[-outcity1] ,popul[-outcity1]))
# El convex hull (casco convexo) de datos bivariados
## Convex hull
hull<-with(USairpollution, chull(manu, popul))
hull
with(USairpollution, plot(manu, popul, pch=1, xlab=mlab, ylab=plab))
with(USairpollution, polygon(manu[hull], popul[hull], density = 15, angle = 30))
with(USairpollution, cor(manu[-hull], popul[-hull]))

# Clase 04 de octubre
USairpollution
pairs(USairpollution, pch = ".", cex=5)
pairs(USairpollution, panel = function(x, y, ...){
  points(x, y, ...)
  abline( lm(y ~ x), col="blue")
}, pch = ".", cex=5)


 install.packages("psych")
 library(psych)
 pairs.panels(USairpollution[,], 
              method = "pearson", # correlation method
              density = TRUE,  # show density plots
              ellipses = TRUE # show correlation ellipses
 )

 
# Clase 10 octubre
# Análisis de componentes principales
# Obtener pocas variables
# Graficar o resumir los datos

## Cuál es la mejor manera de construir un índice informativo del rendimiento general (prueba, examen)?
# Por el promedio ponderado, se puede evitar la estandarización
# Usos del an+alisis de componentes principales: Representación gráfica, otros análisis (de regresión)
    # Existen varias variables explicativas em relacoión con el número de observaciones
    # Las variables explicativas están altamente correlacionadas
demo("Ch-PCA")
blood_corr
blood_sd
blood_pcacov=princomp(covmat=blood_cov)
summary(blood_pcacov, loadings = TRUE)
blood_cov
blood_pcacor=princomp(covmat=blood_corr)
summary(blood_pcacor, loadings = TRUE)

headsize
hdsz=headsize[,c("head1","head2")]
hdsz
summary(hdsz)
hdsz_cov=cov(hdsz)
hdsz_cov
hdsz_corr=cor(hdsz)
hdsz_corr
plot(hdsz)
hdsz_pcacov=princomp(covmat=hdsz_cov)
summary(hdsz_pcacov, loadings = TRUE)
hdsz_pcacor=princomp(covmat=hdsz_corr)
summary(hdsz_pcacor, loadings = TRUE)
pairs(hdsz, panel = function(x, y, ...){
  points(x, y, ...)
  abline(lm(y ~ x), col="blue")
  abline(a=0.71,b=0.71)
}, pch = ".", cex=5)
boxplot(hdsz)



# Clase 11 de octunre
# Contaminaciòn del aire en EEUU
# Analizar varios aspectos de la contaminaciòn (SO_2, temp, ...)
# PCA: Abordar los factores determinantes de la contaminaciòn
# SO_2: contenido de SO_2 en el aire en microgramos por m´3
# Temp: temperatura anual en grados Fahrenheit
# manu: nùmero de emrpesas maniufecactureras que emplean a 20 o màs trabajadores 
# popul: tamaño de la poblaciòn (censo 1970) en miles
# wind: velocidad media anual del viento en millas/hora
# precip: precipitaciòn media anual en pulgadas
# predays: promedio de dìas con precipitaciòn al año
x11()

# de las 6 variables, 2 se relacionan con factores humanos y con 4 con el clima
dfsinSO2=USairpollution[,-1]
usair_so2=princomp(dfsinSO2,cor = TRUE)
summary(usair_so2, loadings = TRUE)
# Se toman los PC cuyos eigenvalores sean >= 1
# NOTA: Podemos vernos sentadps a buscar una nterpretaciòn de los componentes que les permita ser "etiquetados" en algùn sentido
# los coeficientes no importa el signo xq se elevan al cuadrado
# Y_1= podrìa considerarse como un ìndicce de "calidad de vida"
# Y_2= "clima hùmedo"- se relaciona con la cantidad de lluvia en una ciudad con altos coeficientes en precio predays
# Y_3= "tipo de clima" - contraste entre precip y temp separa a las ciudades que tienen temperaturas altas y mucha lluvia de las que son màs frias o màs secas
# ADVERTENCIA (peligros de la sobreinterpretaciòn)
# 1. no existe ningùn mètodo matemàtico diseñado para dar resultado fisisicamente signifcativos
#     Si una expresiòn tiene significado fìsico obvio, dbe atribuirse a un cambio afortunadoo al hecho de que los datos tienen una estructura fuertemente marcada
#     Si no nos importa etiquetar a los componentes principales, aun pueden usarse como base de varias presentaciones gràficas (en este caso de las ciudades)
pairs(usair_so2$scores[,1:3],ylim=c(-6,4),xlim=c(-6,4),panel=function(x,y,...){
  text(x,y,abbreviate(row.names(USairpollution)),cex = 0.6)
   bvbox(cbind(x,y),add=TRUE)
  })
usair_so2$scores
# puntaje que tiene cada variable en los componentes principales ("""nuevos datos""")


# Clase 08 de noviembre

demo("Ch-EFA")
life # los datos muestran la esperanza de vida en años por país, edad y sexo para 1960

sapply(1:3, function(f) factanal(life, factors=f, method="mle") $ PVAL)
factanal(life, factors = 3, method="mle")
# factor 1 - está domnado por la esperanza de vida de un individuo al nacer tanto para hombres como para mujeres ("fuerza de vida al nacer")
# factor 2 - refleja la esperanza de vida en edades más avanzadas ("fuerza de vida de personas mayores")
# factor 3 - tiene cargas más altas para las expectativas de vida de los hombres de 50 y 75 años ("fuerza de vida para hombres mayores de 50 años")


# Clase 14 noviembre
scores=factanal(life, factors = 3, method="mle", scores = "regression")$scores

x11()

scores2=as.data.frame(scores)
plot(Factor2 ~ Factor1, data = scores2, xlab = "Factor 1", ylab = "Factor 2")
plot(Factor2 ~ Factor1, data = scores2, cex.lab=0.9, xlab = "Factor 1", ylab = "Factor 2", type="n")
text(scores2$Factor1, scores2$Factor2, cex=0.8, labels=abbreviate(row.names(scores2)))

plot(Factor3 ~ Factor1, data = scores2, xlab = "Factor 1", ylab = "Factor 3")
plot(Factor3 ~ Factor1, data = scores2, cex.lab=0.9, xlab = "Factor 1", ylab = "Factor 3", type="n")
text(scores2$Factor1, scores2$Factor3, cex=0.8, labels=abbreviate(row.names(scores2)))

plot(Factor3 ~ Factor2, data = scores2, xlab = "Factor 2", ylab = "Factor 3")
plot(Factor3 ~ Factor2, data = scores2, cex.lab=0.9, xlab = "Factor 2", ylab = "Factor 3", type="n")
text(scores2$Factor2, scores2$Factor3, cex=0.8, labels=abbreviate(row.names(scores2)))


druguse
sapply(1:7, function(nf) factanal(covmat = druguse, factors=nf, method="mle", n.obs = 1634) $ PVAL) #se toman los que son menores a 0.05
factanal(covmat = druguse, factors = 6, method="mle",n.obs = 1634)
# Factor 1 uso de drogas sociales (cigarros, cerveza, vino, licor, marihuana)
# Facotr 2 uso de drogas fuertes (coca, tranquilizantes, heroìna)
# Factor 3 uso de anfetaminas 
# Factpr 4 uso de hashish     hasta aqui se explica TODO
# Factor 5 uso de marihuana   NO existe
# Factor 6 uso de inhalantes  NO existe

# Clase 15 de noviembre

validacion=function(fac) {
  fa=factanal(covmat = druguse, factors = fac, method="mle", n.obs = 1634)
  est=tcrossprod(fa$loadings) + diag(fa$uniquenesses)
  ret=round(druguse - est,3) # diferencia de la matriz orginal y la matriz estimada
  ret
}
validacion(4) #nos quedamos con el nùmero que arroje mayor cantidad de 0

life
mlife<-life[,c("m0","m25","m50","m75")]
mlife
sapply(1:1, function(f) factanal(mlife, factors=f, method="mle") $ PVAL)
factanal(mlife, factors = 1, method="mle")
manscores=factanal(mlife, factors = 1, method="mle", scores = "regression")$scores
manscores

wlife<-life[,c("w0","w25","w50","w75")]
wlife
sapply(1:1, function(f) factanal(wlife, factors=f, method="mle") $ PVAL)
factanal(wlife, factors = 1, method="mle")
womanscores=factanal(mlife, factors = 1, method="mle", scores = "regression")$scores
womanscores


# Clase 22 de noviembre

# Algoritmo de agrupamiento de k-means
# Paso 1 - asignar aleatoriamente un nùmero del 1 al k a cada una de las observaciones
# Paso 2 - iterar hasta que las asignaciones de clusters dejen de cambiar:
  # a) para cada no de ls k cluster, calcule el centroide del cluster
    # el centroide es la medida de los valores de los puntos de datos en el k-èsimo cluster
  # b) asigne cada observaciòn al cluster cuyo centroide sea el màs cercano

# Agrupamiento de k medias
# ej, simulado, en el que tenemos dos grupos de los datos:

#con 2 cluster y semilla 2
set.seed(2)
x=matrix(rnorm(50 * 2), ncol = 2)
View(x)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4
km.out=kmeans(x,2,nstart=20)   # agrupamiento de k-medias con k=2
km.out
x11()
par(mfrow=c(1,2))
plot(x, col=(km.out$cluster+2), nmain="Resultados de agrupamiento de k medias con k=2", xlab="", ylab = "", pch=20, cex=2)


#con 3 cluster y semilla 4 
set.seed(4)
km.out=kmeans(x,3,nstart=20)   # agrupamiento de k-medias con k=2
km.out
plot(x, col=(km.out$cluster+2), nmain="Resultados de agrupamiento de k medias con k=3", xlab="", ylab = "", pch=20, cex=2)

set.seed(4)
km.out1=kmeans(x,3,nstart=1)
km.out1$tot.withinss # cambiandole nstart aumento la varianza 

km.out2=kmeans(x,3,nstart = 20)
km.out2$tot.withinss
# si es mayor el nstart hay menos error, menos varianza, y si hay menos en n
# nstart hay mas varianza















"Mariana & ALberto x100pre <3 4ever n ever"
#te amo