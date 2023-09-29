library(faraway)
data(meatspec)
dim(meatspec)
attach(meatspec)
head(meatspec)
training <- meatspec[1:172, ]
test     <- meatspec[173:215, ]


#se ajuta un modelo con las 100 predictoras usando la muestra de entrenamiento.
modelo <- lm(fat ~ ., data = training)
summary(modelo)
#calculamos el error de prediccion en la muestra de entrenamiento
training_mse <- mean((modelo$fitted.values - training$fat)^2)
training_mse

# ahora predecimos la muestra de validacion y calculamos el error de prediccion
predicciones <- predict(modelo, newdata = test)
test_mse <- mean((predicciones - test$fat)^2)
test_mse


# Cálculo de componentes principales.

pca <- prcomp(training[, -101], scale. = TRUE)
round(summary(pca)$importance[, 1:9],4)
#nos quedamos con 4
#calculamos la regresion con las 4
library(pls)
modelo_pcr <- pcr(formula = fat ~ ., data = training, scale. = TRUE, ncomp = 4)

#ahora predecimos con la muestra de validacion
predicciones <- predict(modelo_pcr, newdata = test, ncomp = 4)
test_mse <- mean((predicciones - test$fat)^2)
test_mse

#horrible
#si hacemos validacion cruzada con el error de prediccion
set.seed(123)
modelo_pcr <- pcr(formula = fat ~ ., data = training, scale. = TRUE,
                  validation = "CV")

modelo_pcr_CV <- MSEP(modelo_pcr, estimate = "CV")
plot(modelo_pcr_CV)
which.min(modelo_pcr_CV$val)

par(mfrow = c(1,2))
plot(modelo_pcr_CV$val, main = "MSE vs nº componentes", type = "l",
     ylab = "MSE",
     col = "blue", xlab = "Componentes")
plot(modelo_pcr_CV$val, main = "zoom", type = "l", ylab = "MSE",
     xlab = "Componentes", col = "blue", ylim = c(0,20))

#el optimo es el 20 que se calcula con training. 
predicciones <- predict(modelo_pcr, newdata = test, ncomp = 20)
test_mse <- mean((predicciones - test$fat)^2)
test_mse



library(pls)
set.seed(123)
modelo_pls <- plsr(formula = fat ~ ., data = training, scale. = TRUE, validation = "CV")
modelo_pls_CV <- MSEP(modelo_pls, estimate = "CV")
which.min(modelo_pls_CV$val)
## [1] 15
par(mfrow = c(1,2))
plot(modelo_pls_CV$val, main = "MSE vs nº componentes", type = "l",
     ylab = "MSE",
     col = "blue", xlab = "Componentes")
plot(modelo_pls_CV$val, main = "zoom", type = "l", ylab = "MSE",
     xlab = "Componentes", col = "blue", ylim = c(0,20))


predicciones <- predict(modelo_pls, newdata = test, ncomp = 15)
test_mse <- mean((predicciones - test$fat)^2)
test_mse

