rm(list=ls())
library(pls)

vessel_X <- read.table("Vessel_X.txt",sep = ",")
vessel_Y <- read.table("Vessel_Y.txt",sep = ",")

Y4 <- vessel_Y[,4]

#datos<-cbind(Y4,scale(vessel_X))
datos<-cbind(Y4,vessel_X)

set.seed(1234)
train<-sample(rep(c(TRUE,TRUE,FALSE),nrow(vessel_X)/3),replace=FALSE)
test<-(!train)

training<-datos[train,]
testing<-datos[test,]

#pcr
set.seed(1234)
modelo_pcr <- pcr(formula= Y4 ~ .,data=data.frame(training),scale. = TRUE,validation = "CV")
modelo_pcr_CV <- MSEP(modelo_pcr,estimate= "CV")
plot(modelo_pcr_CV)
which.min(modelo_pcr_CV$val)
modelo_pcr_CV$val
cv_min_pcr<-min(modelo_pcr_CV$val)
cv_min_pcr #MSE min

par(mfrow = c(1,2))
plot(modelo_pcr_CV$val, main = "MSE vs nº de componentes", type = "l",
     ylab = "MSE",
     col = "blue", xlab = "Componentes")
plot(modelo_pcr_CV$val, main = "Zoom", type = "l", ylab = "MSE",
     xlab = "Componentes", col = "blue", ylim = c(0,2))

#error de prediccion
y_p<-predict(modelo_pcr,newdata = testing[,-1],ncomp=102)
error_pcr<-mean((testing[,1]-y_p)^2)
error_pcr
