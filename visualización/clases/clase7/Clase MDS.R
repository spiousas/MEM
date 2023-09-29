ciudades<- read.csv("ciudades.txt", sep="")
ciudades

####################
####################
#A mano

A<- -0.5*ciudades^2
n<-dim(ciudades)[1]
H<-diag(rep(1,n))-n^{-1}*(rep(1,n))%*%t(rep(1,n))
B<-H*A*H
DEL<-eigen(B)$values
round(eigen(B)$values,3)
V<-eigen(B)$vectors

round(B-V%*%diag(DEL)%*%t(V))

V1<-V[,1:2]
t(V1)%*%(V1)
DEL1<-diag(DEL[1:2])
Y<-V1%*%sqrt(DEL1)
plot(Y[,1], -Y[,2])
text(Y[,1], -Y[,2], names(ciudades), cex = 0.9, xpd = TRUE)

V13<-V[,1:3]
t(V13)%*%(V13)
DEL3<-diag(DEL[1:3])
Y3<-V13%*%sqrt(DEL3)

plot(Y[,1], -Y[,2])
text(Y[,1], -Y[,2], names(ciudades), cex = 0.9, xpd = TRUE)

library("scatterplot3d") 

scatterplot3d(Y3,pch=20,angle=50)     
dibu<-scatterplot3d(Y3,pch=20,angle=50)
text(dibu$xyz.convert(Y3),labels=names(ciudades),angle=50)     

#USANDO LIBRERIAS
mds0 <- mds(delta = ciudades,ndim = 2 )
mds0
plot(mds0)

mds1 = cmdscale(ciudades, k = 2)
plot(mds1)
# plot
plot(mds1[,1], mds1[,2], type = "n", xlab = "", ylab = "", axes = FALSE,
     main = "cmdscale (stats)")
text(mds1[,1], mds1[,2], names(ciudades), cex = 0.9, xpd = TRUE)
mds1

#rotadas y reflejadas respescto a su ubicacion geografica
# stress como medida del buen ajuste.
# en este caso es bajo se perdio poco lo que era de esperar

plot(mds0)
plot(-mds0$conf[,1],-mds0$conf[,2])
text(-mds0$conf[,1], -mds0$conf[,2], names(ciudades), cex = 0.9, xpd = TRUE)

library(ggplot2)
ggplot() + geom_point(data = as.data.frame(mds0$conf) , mapping = aes(x = -D1 , y = -D2), alpha = 0.1 , color = "blue", size = 3 ) + geom_text(data = as.data.frame(mds0$conf), mapping = aes(x = -D1,y= -D2), label = rownames(ciudades) ) 



# STRESS
# en mds0$confdist estan las distancias transformadas
#por ejemplo
ciudades[,1]/as.matrix(mds0$confdist)[,1]


dhat_matrix = as.matrix(mds0$dhat)
d_matrix = as.matrix(mds0$confdist)
p_ij = dhat_matrix[upper.tri(dhat_matrix)]
d_ij = d_matrix[upper.tri(d_matrix)]
denominator = sum(p_ij^2)
nominator = sum((p_ij - d_ij)^2) 
normalized_stress = nominator/denominator
normalized_stress

(mds0$stress)*(mds0$stress)


# hay varias librerias
#install.packages(c("vegan", "ecodist", "labdsv", "ape", "ade4", "smacof"))
#library(stats)  
#cmdscale() 
#library(smacof)
#smacofSym() 
#library(vegan)
#wcmdscale() 
#library(ecodist)
#pco()
#library(labdsv)
#pco() 
#library(ape)
#pcoa()
#library(ade4)
#dudi.pco()


#EJEMPLO 2
intensifiers <- readRDS(url("https://tinyurl.com/7k378zcd"))
# numero de ocurrencias
sum(intensifiers)

distancias <- dist(intensifiers, method="canberra", diag=T, upper=T)
head(as.matrix(distancias))
mds <- cmdscale(distancias,eig=TRUE, k=2)
mds


x <- mds$points[,1]
y <- mds$points[,2]
plot(x, y, xlab="Dim.1", ylab="Dim.2", type="n")
text(x, y, labels = row.names(intensifiers), cex=.7)
``