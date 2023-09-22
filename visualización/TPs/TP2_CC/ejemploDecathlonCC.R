#Men's Olympic Decathlon Data from 1988
#Data from men's 1988 Olympic decathlon
#Total of n = 34 athletes
#Have p = 10 variables giving score for each decathlon 
#event Have overall decathlon score
#also (score)
library(ade4)
data(olympic)
aa<-data.frame(olympic$tab)
#write_csv(aa, tmp)

names(olympic)
head(olympic$tab)
summary(olympic$tab)
names(olympic$tab)
# 100 metros (100), salto largo (largo), tiro (poid), salto alto (alto), 400 metros (400),
#obst?culos de 110 metros (110 ), lanzamiento de disco (disq), salto con p?rtiga (perc),
#jabalina (jave) y 1500 metros (1500)

# Variables asociadas con running  (100, 400, 110, y 1500),
# bajos puntos corresponden a mejor performance pero para las otras disciplinas
# es al revez
#Entonces para tener una interpretacion mas facil le cambiamos el signo a estas variables
olympic$tab[,c(1,5,6,10)]<- -olympic$tab[,c(1,5,6,10)]

#Vamos a separar en dos conjuntos de datos
#las actividades que tienen que ver con los brazos
#y las que tiene que ver con las piernas
#X: tiro(poid), disco (disq), javelina (jave), (perc) pole vault
#Y: 100, salto en largo (long), salto en alto (haut), 400, obstaculos (110),1500

X<-olympic$tab[,c( "poid",  "disq" ,"perc", "jave")]
Y<-olympic$tab[,c("100",  "long","haut", "400" , "110",   "1500")]

n <- nrow(X) # n = 33
p <- ncol(X) # p = 4
q <- ncol(Y) # q = 6

library(CCA)
correl <- matcor(X, Y)
correl
img.matcor(correl, type = 2)

# CC a mano (sin estandarizar las variables)
 Sx <- cov(X) #var(X) es lo mismo
 Sy <- cov(Y)
 Sxy <- cov(X,Y)
 Sxeig <- eigen(Sx)
 SxI <- Sxeig$vectors %*% diag(1/Sxeig$values) %*% t(Sxeig$vectors)
 Syeig <- eigen(Sy)
 SyI <- Syeig$vectors %*% diag(1/(Syeig$values)) %*% t(Syeig$vectors)
 Xmat <- SxI %*% Sxy %*% SyI %*% t(Sxy) 
 Ymat <- SyI %*% t(Sxy) %*% SxI %*% Sxy
 Xeig <- eigen(Xmat)
 Yeig <- eigen(Ymat)
 
 Xeig$values
 Yeig$values
 
# Usamos la funcion cancor
ccaXY <- cancor(X, Y)
names(ccaXY)
# o bien
ccXY<-cc(X,Y)
names(ccXY) 

 # comparamos las correlaciones al cuadrado
 Xeig$values
 ccaXY$cor^2
 ccXY$cor^2
 
 # Comparamos las componentes.
 # tenemos que estandarizar los autovectores
 
 XautovecST<-Xeig$vectors%*%diag(1/sqrt(diag( t(Xeig$vectors)%*%Sx%*%Xeig$vectors)))
 YautovecST<-Yeig$vectors%*%diag(1/sqrt(diag( t(Yeig$vectors)%*%Sy%*%Yeig$vectors)))
 XautovecST
 YautovecST
 
 # que coinciden con 
 ccXY$xcoef
 ccXY$ycoef
 
 ccaXY$xcoef*sqrt(n-1)
 ccaXY$ycoef*sqrt(n-1)
 
 
 # La forma mas comoda entonces es con la funcion cc seguimos con esa
 
names(ccXY)
ccXY$xcoef
ccXY$ycoef

plot(ccXY$cor, type = "b")
ccXY$cor
plot(ccXY$scores$xscores[,1],ccXY$scores$yscores[,1])
plot(ccXY$scores$xscores[,2],ccXY$scores$yscores[,2])
plot(ccXY$scores$xscores[,3],ccXY$scores$yscores[,3])
plot(ccXY$scores$xscores[,4],ccXY$scores$yscores[,4])

plt.cc(ccXY,var.label=TRUE)
plt.var(ccXY,d1=1,d2=1,var.label=TRUE)

require(vegan)
cca2 <- CCorA(X,Y)
biplot(cca2)

