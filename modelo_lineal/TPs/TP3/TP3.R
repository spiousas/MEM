pacman::p_load(genridge)

# Ejercicio 3 ####

data <- prostate

## a ####
modelo <- lm(data =  data,  lpsa ~ lcavol + lweight, x = TRUE)

## b ####
X <- as.matrix(modelo$x)
t(X) %*% X

res <- matrix(rep(0, 9), nrow = 3)

for (i in 1:nrow(X)) {
  res <- res + X[i,] %*% t(X[i,])
}

res - t(X) %*% X
# Da igual

## c ###
cor(modelo$fitted.values, data$lpsa)^2
summary(modelo)

# La correlación al cuadrado es el R

## c ###
mean(modelo$residuals)

# El promedio de los residuos es 0

## d ####
summary(modelo)
# El logaritmo del antígeno prostático específico depende linealmente de lcavol y
# lweight con una pendiente de .65 y .66 respectivamente.