#item =c("A","B","C","D")
x1 = c(5,-1,1,-3)
x2 = c(3,1,-2,-2)
data = cbind(x1,x2)
data

#CLUSTERS

#set.seed(2)
X=matrix(data, ncol = 2)
View(X)
#x[1:4,1]=x[1:4,1]
X[1:2,1]=X[1:2,1]
X[1:2,2]=X[1:2,2]
km.out=kmeans(X,2,nstart=256)   # agrupamiento de k-medias con k=2
km.out
km.out$tot.withinss
x11()
par(mfrow=c(1,1))
plot(X, col=(km.out$cluster+2), xlab="", ylab = "", pch=5, cex=2)
