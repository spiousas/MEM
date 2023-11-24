rm(list=ls())
library(FNN)
library(ISLR2)
library(tidyverse)
library(patchwork)
library(tidymodels)
library(caret)
library(Metrics)

data_normales <- read.table("Distrofia-Data_normales.txt")
data_portadoras <- read.table("Distrofia-Data_portadoras.txt")

portadora <- c(rep(0,nrow(data_normales)),rep(1,nrow(data_portadoras)))


data <-cbind(rbind(data_normales,data_portadoras),portadora)[,c(6:7,9:14)]
names(data)<-c("edad","mes","a?o","CK","H","PK","LD","portadora")

boxplot(data$edad ~ data$portadora)
boxplot(data$CK ~ data$portadora)$out
boxplot(data$H ~ data$portadora)$out
boxplot(data$PK ~ data$portadora)$out
boxplot(data$LD ~ data$portadora)$out

data[data$PK==-9999,]
data[data$LD==-9999,]

#saco filas correspondientes a obs que no tienen sentido
data_limpia <- data[!(data$PK==-9999) & !(data$LD==-9999),]

boxplot(data_limpia$edad ~ data_limpia$portadora)

boxplot(data_limpia$CK ~ data_limpia$portadora)
boxplot(data_limpia$H ~ data_limpia$portadora)
boxplot(data_limpia$PK ~ data_limpia$portadora)
boxplot(data_limpia$LD ~ data_limpia$portadora)

par(mfrow=c(1,2))
hist(data_limpia$CK[data_limpia$portadora==0],xlim =
                                c(min(data_limpia$CK),max(data_limpia$CK)))
hist(data_limpia$CK[data_limpia$portadora==1],xlim =
                                c(min(data_limpia$CK),max(data_limpia$CK)))

hist(data_limpia$H[data_limpia$portadora==0],xlim =
                        c(min(data_limpia$H),max(data_limpia$H)))
hist(data_limpia$H[data_limpia$portadora==1],xlim =
                        c(min(data_limpia$H),max(data_limpia$H)))

hist(data_limpia$PK[data_limpia$portadora==0],xlim =
                  c(min(data_limpia$PK),max(data_limpia$PK)))
hist(data_limpia$PK[data_limpia$portadora==1],xlim =
                  c(min(data_limpia$PK),max(data_limpia$PK)))

hist(data_limpia$LD[data_limpia$portadora==0],xlim =
                   c(min(data_limpia$LD),max(data_limpia$LD)))
hist(data_limpia$LD[data_limpia$portadora==1],xlim =
                   c(min(data_limpia$LD),max(data_limpia$LD)))


set.seed(1234)
split <- initial_split(data_limpia, strata = portadora, prop = 2/3)
training <- training(split)
testing <- testing(split)

set.seed(123)
folds<-vfold_cv(training, v = 10,strata = portadora)

###vecinos cercanos
ks<-1:20
pred_knn<-function(K) {
  error_cv<-rep(0,10)
  f1_cv<-rep(0,10)
  for (i in 1:10) {
    fold <-folds$splits[[i]]
    data_train<- analysis(fold)
    data_val<- assessment(fold)
    pred<- knn(train = data_train %>% dplyr::select(CK,H,PK,LD) %>% mutate(across(everything(), scale)), 
                 test = data_val %>% dplyr::select(CK,H,PK,LD) %>% mutate(across(everything(), scale)), 
                 cl = data_train %>% pull(portadora), k = K, prob = TRUE)
    error_cv[i] <- mean(pred != data_val$portadora)
    conf<-confusionMatrix(pred,as.factor(data_val$portadora))
    f1_cv[i] <-conf$table[1,1]/(conf$table[1,1]+1/2*(conf$table[2,1]+conf$table[1,2]))
    #f1_cv[i] <- Metrics::f1(pred,data_val$portadora)
  }
  return(c(mean(error_cv),mean(f1_cv)))
}

cv_error_k <- rep(0,20)
cv_f1_k <- rep(0,20)
for (j in ks) {
  metricas_k<- pred_knn(j)
  cv_error_k[j] <- metricas_k[1]
  cv_f1_k[j] <- metricas_k[2]
}

plot(ks,cv_error_k)
which.min(cv_error_k)
min(cv_error_k)

plot(ks,cv_f1_k)
max(cv_f1_k)
which.max(cv_f1_k)

#pruebo knn con k=15
knn_pred <- knn(train = training %>% select(CK,H,PK,LD) %>% mutate(across(everything(), scale)), 
                test = testing %>% select(CK,H,PK,LD)  %>% mutate(across(everything(), scale)) , 
                cl = training %>% pull(portadora), k = 15, prob = TRUE)

error_knn <- mean(knn_pred != testing$portadora)
accuracy<- Metrics::accuracy(knn_pred,testing$portadora)
precision<- Metrics::precision(knn_pred,testing$portadora)
recall<- Metrics::recall(knn_pred,testing$portadora)
f1<- Metrics::f1(knn_pred,testing$portadora)

conf_knn<-confusionMatrix(knn_pred,as.factor(testing$portadora))
f1_knn<-conf_knn$table[1,1]/(conf_knn$table[1,1]+1/2*(conf_knn$table[2,1]+conf_knn$table[1,2]))

### glm
ps<-seq(0.01,0.99,0.01)
pred_glm<-function(p) {
  error_cv<-rep(0,10)
  f1_cv<-rep(0,10)
  for (i in 1:10) {
    fold <-folds$splits[[i]]
    data_train<- analysis(fold)
    data_val<- assessment(fold)
    ajuste <- glm(portadora ~ CK + H + PK + LD , data = data_train, family = binomial)
    prob <- predict( ajuste, newdata =  data_val %>% select(CK,H,PK,LD), type = 'response')
    pred <- ifelse( prob > p, 1, 0)
    error_cv[i] <- mean(pred != data_val$portadora)
    conf<-confusionMatrix(as.factor(pred),as.factor(data_val$portadora))
    f1_cv[i] <-conf$table[1,1]/(conf$table[1,1]+1/2*(conf$table[2,1]+conf$table[1,2]))
    #f1_cv[i] <- Metrics::f1(pred,data_val$portadora)
  }
  return(c(mean(error_cv),mean(f1_cv)))
}

cv_error_p <- rep(0,length(ps))
cv_f1_p <- rep(0,length(ps))
for (j in 1:length(ps)) {
  metricas_p <-pred_glm(ps[j])
  cv_error_p[j]<- metricas_p[1]
  cv_f1_p[j] <- metricas_p[2]
}

which.min(cv_error_p)
min(cv_error_p)
ps[35]
cv_error_p[35]
which.max(cv_f1_p)
max(cv_f1_p)
plot(ps,cv_f1_p)
cv_f1_p[50]

#pruebo glm con p=0.35
ajuste <- glm(portadora ~ CK + H + PK + LD , data = training, family = binomial)
prob <- predict( ajuste, newdata =  testing %>% select(CK,H,PK,LD), type = 'response')
glm_pred <- ifelse( prob > 0.35, 1, 0)

error_glm <- mean(glm_pred != testing$portadora)
accuracy<- Metrics::accuracy(glm_pred,testing$portadora)
precision<- Metrics::precision(glm_pred,testing$portadora)
recall<- Metrics::recall(glm_pred,testing$portadora)
f1<- Metrics::f1(glm_pred,testing$portadora)

conf_glm<-confusionMatrix(as.factor(glm_pred),as.factor(testing$portadora))
f1_glm<-conf_glm$table[1,1]/(conf_glm$table[1,1]+1/2*(conf_glm$table[2,1]+conf_glm$table[1,2]))
(conf_glm$table[1,1]+conf_glm$table[2,2])/66

##glmnet cv a mano
lambdas<-10 ^ seq(0, -3, length = 100)
pred_glm_lasso<-function(p,lambda) {
  error_cv<-rep(0,10)
  f1_cv<-rep(0,10)
  for (i in 1:10) {
    fold <-folds$splits[[i]]
    data_train<- analysis(fold)
    data_val<- assessment(fold)
    ajuste <- glmnet(as.matrix(data_train[,4:7]), as.matrix(data_train[,8]), lambda = lambda ,family = "binomial")
    prob <- predict( ajuste, newx=as.matrix(data_val[,4:7]), s=lambda,type = 'response')
    pred <- ifelse( prob > p, 1, 0)
    error_cv[i] <- mean(pred != data_val$portadora)
    conf<-confusionMatrix(as.factor(pred),as.factor(data_val$portadora))
    f1_cv[i] <-conf$table[1,1]/(conf$table[1,1]+1/2*(conf$table[2,1]+conf$table[1,2]))
    #f1_cv[i] <- Metrics::f1(pred,data_val$portadora)
  }
  return(c(mean(error_cv),mean(f1_cv)))
}

error_clas<-matrix(0,nrow=length(ps),ncol=length(lambdas))
f1_clas<-matrix(NA,nrow=length(ps),ncol=length(lambdas))
for (j in 1:length(ps)) {
  for (l in 1:length(lambdas)) {
    metricas_p <-pred_glm_lasso(ps[j],lambdas[l])
    error_clas[j,l]<-metricas_p[1]
    f1_clas[j,l]<-metricas_p[2]
  }
}

max(f1_clas)
which(f1_clas==max(f1_clas),arr.ind = TRUE)
ps[43]
lambdas[84]

#glmnet
cvfit = cv.glmnet(as.matrix(training[,4:7]), as.matrix(training[,8]), family = "binomial", type.measure = "class")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
print(cvfit)

pred_glmnet <-predict(cvfit, newx = as.matrix(testing[,4:7]), s = "lambda.min", type = "response")
error_glmnet <- mean(pred_glmnet != testing$portadora)
accuracy<- Metrics::accuracy(pred_glmnet,testing$portadora)
conf_glmnet<-confusionMatrix(as.factor(pred_glmnet),as.factor(testing$portadora))
f1_glmnet<-conf_glmnet$table[1,1]/(conf_glmnet$table[1,1]+1/2*(conf_glmnet$table[2,1]+conf_glmnet$table[1,2]))
(conf_glmnet$table[1,1]+conf_glmnet$table[2,2])/66

###
set.seed(234)
DMD_fold <- vfold_cv(DMD_train, strata = portadora)
DMD_fold
####

##
conf<-confusionMatrix(knn_pred,as.factor(testing$portadora))
conf$table[1,1]
conf$table[1,1]/(conf$table[1,1]+1/2*(conf$table[2,1]+conf$table[1,2]))
(conf$table[1,1]+conf$table[2,2])/66
##