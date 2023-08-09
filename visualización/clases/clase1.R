library(here)
library(readr)

## a ####
data_pbi <- read_csv(here("Documentos/Spiousas/data/PBI.csv"))
x <- as.matrix(data_pbi)

## b ####
pairs(x, pch=20)  

## c ####
media_muestral <- colMeans(x)

n <- 26
I <- diag(rep(1, n))
unos <- matrix(1, n, 1)

H <- I - 1/n * unos %*% t(unos)
x_cent <- H %*% x

pairs(x_cent, pch = 20)

## d ####
Q <- t(x_cent) %*% x_cent
Q # Es la matriz de suma de cuadrados
s <- 1/(n-1) * Q # Es la matriz de covarianza

## e ####
D_12 <- diag(sqrt(1/diag(s)))
D_12

Z <- H %*% x %*% D_12 # Matriz de datos estandarizada

pairs(Z)
cov(Z)
