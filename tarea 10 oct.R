# Graphical Techniques

# Figura 1.1
x11=c(3,4,2,6,8,2,5)
x12= c(5, 5.5, 4, 7, 10, 5, 7.5)
x11
x12
plot(x11, x12)

# Figura 1.2
x21=c(5,4,6,2,2,8,3)
x22= c(5, 5.5, 4, 7, 10, 5, 7.5)
x21
x22
plot(x21, x22)


#EJERCICIO 1.1
mean(x11)
var(x11)
mean(x12)
var(x12)
cov(x11,x12)


#Ejercicio 1.2
x31= c(1,2,3,3,4,5,6,8,9,11)
x32= c(18.95, 19, 17.95, 15.54, 14, 12.95, 8.94, 7.49, 6, 3.99)

# a) Construye un gráfico de dispersión de los datos y los diagramas de puntos marginales
plot(x31,x32)

# b) Infiera el signo de la covarianza de la muestra del gráfico de dispersión 
# La covarianza tiene signo negativo

# Medias, varianzas, covariazna y correlación
mean(x31)
var(x31)
mean(x32)
var(x32)
cov(x31,x32)
cor(x31,x32)


# Ejemplo 1.8 página 20 del libro
dnsty=c(.801,.824,.841,.816,.84,.842,.82,.802,.828,.819,.826,.802,.81,.802,.832,.796,.759,.77,.759,.772,.806,.803,.845,.822,.971,.816,.836,.815,.822,.822,.843,.824,.788,.782,.795,.805,.836,.788,.772,.776,.758)
md=c(121.41,127.7,129.2,131.8,135.1,131.5,126.7,115.1,130.8,124.6,118.31,114.2,120.3,115.7,117.51,109.81,109.1,115.1,118.31,112.6,116.2,118,131,125.7,126.1,125.8,125.5,127.8,130.5,127.9,123.9,124.1,120.8,107.4,120.7,121.91,122.31,110.60,103.51,110.71,113.8)
cd=c(70.42,72.47,78.2,74.89,71.21,78.39,69.02,73.1,79.28,76.48,70.25,72.88,68.23,68.12,71.62,53.1,50.85,51.68,50.6,53.51,56.53,70.7,74.35,68.29,72.1,70.64,76.33,76.75,80.33,75.68,78.54,71.91,68.22,54.42,70.41,73.68,74.93,53.52,48.93,53.67,52.42)
df=cbind(dnsty,md,cd)
df
pairs(df, panel = function(x, y, ...){
  points(x, y, ...)
  abline( lm(y ~ x), col="blue")
}, pch = ".", cex=5)



xlim<-with(df,range(dnsty))*1.1
plot(cd ~ dnsty, data = df, cex.lab=0.9, xlab = , ylab = , type="n",xlim=xlim)
with(df, text(dnsty, cd, cex=0.5, labels=abbreviate(row.names(df))))


hist(df$dnsty, main = "")
boxplot(df$dnsty)
otcty=