# DATOS INSECTOS
####################################################################################
# identificar a que especie (a o b) pertenece un determinado insecto.
#Para ello se han medido tres variables (longitud de las patas, diámetro del abdomen y diámetro
#del órgano sexual) en 10 individuos de cada una de las dos especies.

input <- ("
especie pata abdomen organo_sexual 
a 191 131 53
a 185 134 50
a 200 137 52
a 173 127 50
a 171 128 49
a 160 118 47
a 188 134 54
a 186 129 51
a 174 131 52
a 163 115 47
b 186 107 49
b 211 122 49
b 201 144 47
b 242 131 54
b 184 108 43
b 211 118 51
b 217 122 49
b 223 127 51
b 208 125 50
b 199 124 46
")
datos <- read.table(textConnection(input), header = TRUE)
especie<-as.factor(datos$especie)
datos$especie<-especie

library(ggplot2)
library(ggpubr)
p1 <- ggplot(data = datos, aes(x = pata, fill = especie)) +
  geom_histogram(position = "identity", alpha = 0.5)
p2 <- ggplot(data = datos, aes(x = abdomen, fill = especie)) +
  geom_histogram(position = "identity", alpha = 0.5)
p3 <- ggplot(data = datos, aes(x = organo_sexual, fill = especie)) +
  geom_histogram(position = "identity", alpha = 0.5)
ggarrange(p1, p2, p3, nrow = 3, common.legend = TRUE)


# si miramos cada variable separada
#la longitud de la pata parece ser la variable que más se diferencia
#entre especies (menor solapamiento entre poblaciones).
library(GGally) #extencion de ggplot
ggpairs(datos[, -1], aes(colour = datos$especie, alpha = 0.4))


#los siguentes son para ver opciones del ggpairs
ggpairs(datos[, -1])
ggpairs(datos,  upper = list(continuous = "density", combo = "box"),
  lower = list(continuous = "points", combo = "dot"))

par(mfrow=c(1,1)) 
library(scatterplot3d)
colors <- c("#E69F00","#56B4E9")
colors <- colors[as.numeric(especie)]
scatterplot3d(datos[, -1], pch = 16, color=colors)
legend("topleft",
      bty = "n", cex = 0.8,
      title = "Especie",
      c("a", "b"), fill = c("#E69F00","#56B4E9"))



# hacemos bayes naive
library(naivebayes)
# creamos el modelo de pronostico
BayesN <- naive_bayes(especie ~ pata+abdomen+organo_sexual, data = datos)
# bayes naive asume que cada variable interviene en la clasificacion independientemente de las otras
# si no ponemos nada asume normalidad en cada variable

plot(BayesN)

#aca se estima con nucleos cada densidad
BayesNK <- naive_bayes(especie ~ pata+abdomen+organo_sexual, data = datos,usekernel=TRUE)
plot(BayesNK)

datos1<-datos
datos1$p=predict(BayesN)    
head(datos1)

datos2<-datos
datos2$p=predict(BayesNK)    
head(datos2)

#predecir uno nuevo
nuevo<-data.frame(pata = 194, abdomen = 124,  organo_sexual = 49)
predict(BayesN,nuevo)    
predict(BayesNK,nuevo)    
#la prediccion no es igual para ese punto

#probabilidades de cada clase
predict(BayesN,type="prob") #para cada individuo la probabilidad de pertenecer a cada clase
predict(BayesN,nuevo,type="prob")
prediccionesBN<-predict(object = BayesN, newdata = datos[, -1])#,  method = "predictive")
table(datos$especie, prediccionesBN,dnn = c("Clase real", "Clase predicha"))

prediccionesBNK<-predict(object = BayesNK, newdata = datos[, -1])#,  method = "predictive")
table(datos$especie, prediccionesBNK,dnn = c("Clase real", "Clase predicha"))

predict(BayesN,nuevo,type="prob") #para cada individuo la probabilidad de pertenecer a cada clase
predict(BayesNK,nuevo,type="prob") #para cada individuo la probabilidad de pertenecer a cada clase


#LDA
#para seguir con LDA deberia chequear normalidad multidimensional.
####################################################################


# Representación de cuantiles normales de cada variable para cada especie 
par(mfrow=c(2,3)) 
for (k in 2:4) {
  j0 <- names(datos)[k]
  x0 <- seq(min(datos[, k]), max(datos[, k]), le = 50)
  for (i in 1:2) {
    i0 <- levels(especie)[i]
    x <- datos[especie == i0, j0]
    qqnorm(x, main = paste("especie", i0, j0), pch = 19, col = i + 1)
    qqline(x)
  }
}
par(mfrow=c(2,3)) 

for (k in 2:4) {
  j0 <- names(datos)[k]
  x0 <- seq(min(datos[, k]), max(datos[, k]), le = 50)
  for (i in 1:2) {
    i0 <- levels(especie)[i]
    x <- datos[especie == i0, j0]
    hist(x, main = paste("especie", i0, j0), pch = 19, col = i + 1)
      }
}

#test para normalidad multivariada
library(MVN)
library(dplyr)
par(mfrow=c(1,2)) 
datosa<-filter(datos, especie == "a")
mvn(datosa[,-1], mvnTest = "hz", multivariateOutlierMethod = "quan")
datosb<-filter(datos, especie == "b")
mvn(datosb[,-1], mvnTest = "hz", multivariateOutlierMethod = "quan")
# norechazo normalidad

#test para igualdad de varianza
library(HDtest)
testCov(datos[1:10, -1],datos[11:20,-1])
#no rechazo igualdad de varianza

library(MASS)
LDA <- lda(formula = especie ~ pata + abdomen + organo_sexual,
                  data = datos)

predict(object = LDA, newdata = nuevo)
predicciones <- predict(object = LDA, newdata = datos[, -1],  method = "predictive")


# errores de clasificacion
table(datos$especie, predicciones$class,
      dnn = c("Clase real", "Clase predicha"))

trainig_error <- mean(datos$especie != predicciones$class) * 100
trainig_error

LDA

#coordinadas discriminantes
LDA[[4]]

sigma<-(cov(datosa[,-1])+cov(datosb[,-1]))/2
xbar1<-as.vector(LDA[[3]][1,])
xbar2<-as.vector(LDA[[3]][2,])
coordisc<-solve(sigma)%*%(xbar1-xbar2)
coordisc
LDA[[4]]

#no da lo mismo
LDA[[4]]/coordisc

#porque el paquete LDA escala diferente normalizando 
#para que dentro de cada grupo la matriz de covarianza sea esferica

#ordenada al origen
b0<-  as.numeric(-(t(xbar1)%*%solve(sigma)%*%(xbar1)-t(xbar2)%*%solve(sigma)%*%(xbar2)  ) /2 )


predict(object = LDA, newdata = nuevo)
t(coordisc)%*%t(nuevo)+b0 # es negativo entonces se predice b

par(mfrow=c(1,1)) 
scatterplot3d(datos$pata, datos$abdomen, datos$organo_sexual,color = c("#E69F00","#56B4E9")[especie],
pch = 19, grid = TRUE, xlab = "pata", ylab = "abdomen",zlab = "organo sexual", angle = 65, cex.axis = 0.6)


library(klaR)
#muestra la clasificación
#de las observaciones basadas en métodos de clasificación (por ejemplo, 
#lda, qda) para cada combinación de dos variables. 
#Además, se muestran los límites de clasificación y las tasas de error 
#aparentes se dan en cada título
partimat(especie ~  .,data = datos, method = "lda", prec = 200,
         image.colors = c("#E69F00","#56B4E9"),
         col.mean = "gray")


#########################
# QDA
QDA <- qda(formula = especie ~ pata + abdomen + organo_sexual,
           data = datos)

predict(object = QDA, newdata = nuevo)
predicciones <- predict(object = QDA, newdata = datos[, -1],  method = "predictive")


# errores de clasificacion
table(datos$especie, predicciones$class,
      dnn = c("Clase real", "Clase predicha"))

# hago lo mismo pero con cuadratica
partimat(especie ~  .,data = datos, method = "qda", prec = 200,
         image.colors = c("#E69F00","#56B4E9"),
         col.mean = "gray")


svm_model<- 
  svm(especie ~ ., data = datos, type = "C-classification", 
      kernel = "linear", scale = FALSE)
plot(x = svm_model, data = datos)

datossinab<-datos[,-3]
svm_model<- svm(especie ~ .,data = datossinab, kernel = "linear")
plot(svm_model, datossinab)

svm_model<- svm(especie ~ .,data = datossinab, kernel = "polynomial")
plot(svm_model, datossinab)


############################
# SVM
############################

library(e1071)
data(iris)

# fit binary C-classification model
setosa <- as.factor(iris$Species == "setosa")
iris2 = scale(iris[,-5])
m <- svm(setosa ~ Petal.Width + Petal.Length,
         data = iris2, kernel = "linear")

# plot data and separating hyperplane
plot(Petal.Length ~ Petal.Width, data = iris2, col = setosa)
(cf <- coef(m))
abline(-cf[1]/cf[3], -cf[2]/cf[3], col = "red")

# plot margin and mark support vectors
abline(-(cf[1] + 1)/cf[3], -cf[2]/cf[3], col = "blue")
abline(-(cf[1] - 1)/cf[3], -cf[2]/cf[3], col = "blue")
points(m$SV, pch = 5, cex = 2)

#otro

n <- 400
set.seed(1)

df <- data.frame(x1 = runif(n, min = -1, max = 1), 
                 x2 = runif(n, min = -1, max = 1))
radius <- 0.8
radius_squared <- radius^2
df$y <- factor(ifelse(df$x1^2 + df$x2^2 < radius_squared, -1, 1), levels = c(-1, 1))
ggplot(data = df, aes(x = x1, y = x2, color = y)) + 
  geom_point() +
  scale_color_manual(values = c("red", "blue"))

modelo1<- svm(y ~ ., data = df, type = "C-classification", 
              kernel = "polynomial", degree = 2) 

df1 <- data.frame(x1sq = df$x1^2, x2sq = df$x2^2, y = df$y)
ggplot(data = df1, aes(x = x1sq, y = x2sq, color = y)) + 
  geom_point()+ guides(color = FALSE) + 
  scale_color_manual(values = c("red", "blue"))
modelo2<- svm(y ~ ., data = df1, type = "C-classification", 
              kernel = "linear", degree = 2) 
plot(modelo2,df1) 
plot(modelo1,df )
