rm(list=ls())
library(glmnet)
library(leaps)

vessel_X <- read.table(here("taller_de_datos/entrega1/data/Vessel_X.txt"),sep = ",")
vessel_Y <- read.table(here("taller_de_datos/entrega1/data/Vessel_Y.txt"),sep = ",")

Y4 <- vessel_Y[,4]
medias_X <- colMeans(vessel_X)

S<-var(vessel_X)
varianzas_X <-diag(S)

frec<-seq(1,301,1)
plot(frec+99,medias_X)
plot(frec+99,varianzas_X)

plot(frec+99,vessel_X[1,],type = "l",col="red")
lines(frec+99,vessel_X[30,],type = "l",col="blue")
lines(frec+99,vessel_X[60,],type = "l",col="green")
lines(frec+99,vessel_X[90,],type = "l",col="magenta")
lines(frec+99,vessel_X[120,],type = "l",col="grey")
lines(frec+99,vessel_X[150,],type = "l",col="orange")
lines(frec+99,vessel_X[180,],type = "l")

ajuste<-lm(Y4 ~ (.),data=vessel_X)
summary(ajuste)

#lasso
ajuste1<-cv.glmnet(x=as.matrix(vessel_X), y=Y4)
plot(ajuste1)
ajuste1$lambda.min
ajuste1$lambda.1se
coef(ajuste1, s="lambda.1se")

#ridge
ajuste2<-cv.glmnet(x=as.matrix(vessel_X), y=Y4,alpha=0)
plot(ajuste2)
ajuste2$lambda.1se
coef(ajuste2, s="lambda.1se")

#elastic net
ajuste3<-cv.glmnet(x=as.matrix(vessel_X), y=Y4,alpha=0.5)
plot(ajuste3)
coef(ajuste3, s="lambda.1se")

###train y test
set.seed(12)
train<-sample(rep(c(TRUE,TRUE,FALSE),nrow(vessel_X)/3),replace=FALSE)
test<-(!train)

muestra_train<-vessel_X[train,]
y_train<-Y4[train]
muestra_test<-vessel_X[test,]
y_test<-Y4[test]

#modelo 1
ajuste1<-cv.glmnet(x=as.matrix(muestra_train), y=y_train)
y_p1<-predict(ajuste1,newx = as.matrix(muestra_test),s="lambda.1se")
error1<- mean((y_test-y_p1)^2)
error1

#modelo 2
ajuste2<-cv.glmnet(x=as.matrix(muestra_train), y=y_train,alpha=0)
y_p2<-predict(ajuste2,newx = as.matrix(muestra_test),s="lambda.1se")
error2<- mean((y_test-y_p2)^2)
error2

#modelo 3
ajuste3<-cv.glmnet(x=as.matrix(muestra_train), y=y_train,alpha=0.5)
y_p3<-predict(ajuste3,newx = as.matrix(muestra_test),s="lambda.1se")
error3<- mean((y_test-y_p3)^2)
error3

