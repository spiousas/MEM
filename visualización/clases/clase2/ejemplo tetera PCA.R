

library(scatterplot3d)
library(misc3d)

data(teapot)
class(teapot) # es una lista que contiene los vertices y los ejes
dat <- matrix(unlist(teapot[[1]]), ncol = 3, byrow = TRUE)
dat<-cbind(dat[ , 1], dat[ , 3], dat[ , 2])

  
head(dat)
scatterplot3d(dat, highlight.3d = TRUE, angle = 75, pch = 19,  xlab = "", ylab = "", zlab = "", main = "Tetera 3D")
# PCA
(eigenval <- eigen(cov(dat))$values)
(eigenvec <- eigen(cov(dat))$vectors)
PCA_2 <- dat %*% eigenvec[ , 1:2] # proyecto en las dos primeras direcciones
plot(PCA_2,  pch = 19, col= "blue", xlab = "", ylab = "", main = "Tetera en 2D")
round(cumsum(eigenval)/sum(eigenval) * 100, 2)

eigenvecsig <- eigenvec
eigenvecsig[,2] <- -eigenvec[,2]
PCA_sig <- dat %*% eigenvecsig[ , 1:2] # proyecto en las dos primeras direcciones
plot(PCA_sig,  pch = 19, col= "blue", xlab = "", ylab = "", main = "Tetera en 2D")

#como queda la reconstrucion en dimension 3
pc.scores12 <- t(eigenvec[,1:2]) %*% t(dat)
reconstruccion <- t(eigenvec[,1:2] %*%pc.scores12)
library(rgl)
plot3d(reconstruccion,zlim=c(-1,5),col="red")# highlight.3d = TRUE, angle = 75, pch = 19,  xlab = "", ylab = "", zlab = "", main = "Tetera 3D")

#como queda la reconstrucion en dimension 3
pc.scores11 <- t(eigenvec[,1]) %*% t(dat)
reconstruccion1 <- t(eigenvec[,1] %*%pc.scores11)
head(reconstruccion1)
library(rgl)
plot3d(reconstruccion1,zlim=c(-1,5),ylim=c(-4,4),col="blue")# highlight.3d = TRUE, angle = 75, pch = 19,  xlab = "", ylab = "", zlab = "", main = "Tetera 3D")


#como queda la reconstrucion en dimension 3
pc.scores13 <- t(eigenvec[,1:3]) %*% t(dat)
reconstruccion3 <- t(eigenvec[,1:3] %*%pc.scores13)
head(reconstruccion3)
library(rgl)
plot3d(reconstruccion3,zlim=c(-1,5))# highlight.3d = TRUE, angle = 75, pch = 19,  xlab = "", ylab = "", zlab = "", main = "Tetera 3D")

