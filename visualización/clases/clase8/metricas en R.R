library(naivebayes) #para bayes naive
library(rpart)   # para arbol decision
library(rattle)  # para data set, y arbol decision
library(ROCR)    # para curva ROC

#el conjunto weather dataset contiene datos diarios de un anio
# de una estacion meteorologica de Canberra

datos <- weather

#los datos estan en R aca los guardo solo para que esten en un archivo para quien los necesite
library(utils)
dir<-getwd()
write.table(datos, file = "weather.csv", sep = ",",  qmethod = "double")

names(datos)
#borra variables que no uso
datos         <- within(datos, rm("Date","Location","RISK_MM","WindGustDir", "WindDir9am","WindDir3pm"   )) 


#  fija la secuencia de numeros aleatorios
set.seed(42)  
#elejimos un 70% de datos
muestraentre   <- sample(nrow(datos),(nrow(datos)*.7)) 
entrenamiento <- datos[muestraentre,]
validacion <- datos[-muestraentre,]



# usando bayes naive
#------------------------------------------------------------------------------
BayesNK <- naive_bayes(RainTomorrow ~ ., data = entrenamiento,usekernel=TRUE)
prediccionesBN<-predict(object = BayesNK, validacion[,-18])
table(validacion$RainTomorrow, prediccionesBN,dnn = c("Clase real", "Clase predicha"))

predict.BN <- predict(BayesNK,validacion,type = "prob")[,2] #solo hay que pasarle la probabilidad de la clase=yes
predict.rocr  <- prediction (predict.BN,validacion$RainTomorrow)
perf.rocr     <- performance(predict.rocr,"tpr","fpr") #True y False postivie.rate

aucBN <- as.numeric(performance(predict.rocr ,"auc")@y.values)
aucBN
plot(perf.rocr,colorize=TRUE,type="l", main = paste('Area Bajo la Curva BN =',round(aucBN,2)))  
abline(a=0,b=1)



#o bien con la libreria
library(pROC)
objroc <- roc(validacion$RainTomorrow, predict.BN,auc=T,ci=T)
plot.roc(objroc,print.auc=T,print.thres = "best",
         col="blue",xlab="1-ESpecificidad",ylab="Sensibilidad")
#da el AUC con su intervalo de confianza
#el corte optimo con la especificidad y sensibilidad.
SensEspec <-objroc$sensitivities + objroc$specificities 
maximo    <- max(SensEspec)
numOrdenCutoff <- which(SensEspec==maximo)
objroc$thresholds[numOrdenCutoff]

cbind(objroc$thresholds,objroc$sensitivities,objroc$specificities)[100:106,]


library(ROCit)
## Warning: package 'ROCit' was built under R version 3.5.2
ROCit_obj <- rocit(score= predict.BN,class=validacion$RainTomorrow)
plot(ROCit_obj)


# usando arboles de decision
#------------------------------------------------------------------------------
Arboles  <- rpart(RainTomorrow ~ .,entrenamiento, method="class",
                  parms=list(split="information"))
predict.arboles <- predict(Arboles,validacion[,-18],type = "prob")[,2] #prob. clase=yes
predict.rocarboles  <- prediction (predict.arboles,validacion$RainTomorrow)
perf.arboles     <- performance(predict.rocarboles,"tpr","fpr") #True y False postivie.rate

aucarboles <- as.numeric(performance(predict.rocarboles ,"auc")@y.values)
plot(perf.arboles,type='o', main = paste('Area Bajo la Curva Arboles =',round(aucarboles,2)))  
abline(a=0, b= 1)


objrocA <- roc(validacion$RainTomorrow, predict.arboles,auc=T,ci=T)
plot.roc(objrocA,print.auc=T,print.thres = "best",
         col="blue",xlab="1-ESpecificidad",ylab="Sensibilidad")


