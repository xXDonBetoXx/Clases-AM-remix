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



