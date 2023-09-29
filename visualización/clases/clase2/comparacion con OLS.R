# ejemplo de juguete
set.seed(123)
x <- 1:100
xc<-x-mean(x)
e <- rnorm(100, 0, 50)
y <- 20 + 3 * xc + e
yc<-y-mean(y)
xc<-xc/sd(xc)
yc<-yc/sd(yc)

datosc<-cbind(xc,yc)
par(mar=c(2,2,2,2))
plot(datosc,xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),pch=20)
yx.lm <- lm(yc ~ xc)
lines(xc, predict(yx.lm), col="red")
n=12; segments(xc[n],yc[n],xc[n],predict(yx.lm)[n],col="red",lty=2)
n=70; segments(xc[n],yc[n],xc[n],predict(yx.lm)[n],col="red",lty=2)

xy.lm <- lm(xc ~ yc)
lines(predict(xy.lm), yc, col="blue")
n=12; segments(predict(xy.lm)[n],yc[n],xc[n],yc[n],col="blue",lty=2)
n=70; segments(predict(xy.lm)[n],yc[n],xc[n],yc[n],col="blue",lty=2)

res.pca <- prcomp(datosc, scale = TRUE)
res.pca$center
res.pca$scale
res.pca$rotation
ab<-res.pca$rotation[2,1]/res.pca$rotation[1,1]
abline(0,ab,col="darkgreen",lwd=2)


procc<-function(p1,p2)
{
  mu1<-mean(xc)
  mu2<-mean(yc)
  s<-(ab*p2-ab*mu2+ab^2*mu1+p1)/(1+ab^2)
  c(s,ab*(s-mu1)+mu2)
}
n<-12
prx<-procc(xc[n],yc[n])
segments(xc[n],yc[n],prx[1],prx[2],col="darkgreen",lty=2)
n<-70
prx<-procc(xc[n],yc[n])
segments(xc[n],yc[n],prx[1],prx[2],col="darkgreen",lty=2)


## lo hago animado

library(animation)
library(pls)
library(mgcv)
library(MASS)
library(RCurl)
library(bitops)
library(Matrix)
library(lattice)
library(zoo)

###
# Ejemplo de juguete con animacion
##

set.seed(123)
x <- 1:100
xc<-x-mean(x)
e <- rnorm(100, 0, 50)
y <- 20 + 3 * xc + e
yc<-y-mean(y)
xc<-xc/sd(xc)
yc<-yc/sd(yc)

recta<-function(ab,s)
{
  mu1<-mean(xc)
  mu2<-mean(yc)
  ab*(s-mu1)+mu2
}
procc<-function(ab,punto)
{
  p1<-punto[1]
  p2<-punto[2]
  mu1<-mean(xc)
  mu2<-mean(yc)
  s<-(ab*p2-ab*mu2+ab^2*mu1+p1)/(1+ab^2)
  c(s,ab*(s-mu1)+mu2)
}
proccab<-function(ab)
{proccdatos<-datosc
for(i in 1:100)
{
  proccdatos[i,]<-procc(ab,datosc[i,])
}
proccdatos
}

setwd("C:/Users/0dani/Dropbox/daniela/docencia/visualizacion/clases/6PCA")

# guarda un archivo gif.
saveGIF(
  {
    oopt = ani.options(interval = 0.5, nmax =50)
    nmax = ani.options("nmax")
    abseq = seq(0, 3, length = nmax)
    rxyss = rep(NA, nmax)
    ryxss = rep(NA, nmax)
    ross = rep(NA, nmax)
    par(mfrow = c(1,2))
    for (i in 1:nmax)
    {
      dev.hold()
      datosc<-cbind(xc,yc)
      plot(datosc,xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),pch=20,xlab="x",ylab="y")
      yx.lm <- lm(yc ~ xc)
      lines(xc, predict(yx.lm), col="red")
      xy.lm <- lm(xc ~ yc)
      lines(predict(xy.lm), yc, col="blue")
      res.pca <- prcomp(datosc, scale = TRUE)
      res.pca$center
      res.pca$scale
      res.pca$rotation
      ab<-res.pca$rotation[2,1]/res.pca$rotation[1,1]
      abline(0,ab,col="darkgreen",lwd=2)
      abline(0,abseq[i],col=4,lwd=2)
      #text(-1,12,"Areas",font=2)
      #text(-1,c(11,10,9,8,7,6) ,dist*dist,font=2,col=rainbow(11, start = 0.7, end = 0.5))
      #text(-1.4,c(10.5,9.5,8.5,7.5,6.5) ,"+",font=2)
      #text(-1,5.5 ,"________",font=2)
      #text(-1,5,round(sum(dist*dist),3),font=2)
      distxy<-(yc-(abseq[i]*xc+mean(yc)))^2
      distyx<-(xc-((yc-mean(yc))/abseq[i]+mean(xc)))^2
      aux<-proccab(abseq[i])
      disto<-((xc-aux[,1])^2+(yc-aux[,2])^2)
      rxyss[i] = sum(distxy)
      ryxss[i] = sum(distyx)
      ross[i] = sum(disto)
      plot(1:nmax, rxyss, xlab = paste("pendiente=", round(abseq[i],3)), ylab = "",ylim=c(5,50), pch = 20, type = "o",col="red")
      abline(v=min(rxyss),col="red")     
      abline(v=min(ryxss),col="blue")     
      abline(v=min(ross),col="darkgreen")     
      lines(1:nmax, ryxss, pch = 20, type = "o",col="blue")
      lines(1:nmax, ross , pch = 20, type = "o",col="darkgreen")
      
}
    
    flush.console()
    ani.pause()
  } ,movie.name=   "pcaolspeli.gif",ani.height =600, ani.width = 1000,,outdir = getwd())




