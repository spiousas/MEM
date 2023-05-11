library(here)

# Ejercicio 1 ####
## 1 ####
data_astro <- read.table(here("aprendizaje_estadistico/Clase_4/data/astro.txt"), header=T, skip=1)
head(data_astro)

## 2 ####
P <- sum(data_astro$f<=40)/nrow(data_astro)
P

# Uso a la frecuencia relativa como estimador de la probabilidad

## 3 ####
fluxEmp <- ecdf(data_astro$f)
par(mfrow=c(1,1)) 
plot(fluxEmp)

## 4 ####
hist(data_astro$f, freq = F, breaks = 10)

# Pareciera ser una explonencial

## 5 ####
# 1/lambda es la esperanza, entonces aprovecho eso y calculo al promedio, como estimador de la esperanza
# entonces, lambda me queda 1/mean()
lambda <- 1/mean(data_astro$f)
x <- 1:140

# Densidad
hist(data_astro$f, freq = F, breaks = 10)
lines(x, lambda*exp(-x*lambda))

# Distribucion
plot(fluxEmp)
lines(x, 1-exp(-x*lambda))

# Ejercicio 2 ####
## 6 ####
data_buffalo <- scan(here("aprendizaje_estadistico/Clase_4/data/buffalo.txt"))
data_buffalo

par(mfrow=c(3,1))
hist(data_buffalo, freq = F)
hist(data_buffalo, freq = F, xlim = c(20, 140), breaks = seq(20, 140, 10))
hist(data_buffalo, freq = F, xlim = c(20, 140), breaks = seq(20, 140, 5))

# Tiene un efecto, se vuelve menos suave

## 7 ####
par(mfrow=c(3,1))
hist(data_buffalo, freq = F, xlim = c(20, 140), breaks = seq(20, 140, 10))
hist(data_buffalo, freq = F, xlim = c(20, 140), breaks = seq(22, 142, 10))
hist(data_buffalo, freq = F, xlim = c(20, 140), breaks = seq(24, 144, 10))

# Tiene un efecto, no especificamente un suavizado

## 8 ####
proba_est <- function(x, x_0, h) {
  sum(data_buffalo<= x_0+h & data_buffalo>= x_0-h) / (length(data_buffalo))
}
proba_est(data_buffalo, 75, 5)

## 9 ####
proba_est(data_buffalo, 80, 10)
proba_est(data_buffalo, 80, 15)
proba_est(data_buffalo, 80, 20)
proba_est(data_buffalo, 40, 10)
proba_est(data_buffalo, 40, 15)
proba_est(data_buffalo, 40, 20)
# Es mas grande la de 80,10 que la de 40,10

## 10 ####
densidad_est_parzen <- function(x, x_0, h) {
  parzen <- 0*x_0 # Inicializo un vecto de ceros
  for (i in 1:length(x_0)) {
    parzen[i] <- sum(data_buffalo<= x_0[i]+h & data_buffalo>= x_0[i]-h) / (length(data_buffalo)*2*h)
  }  
  parzen
}

## 11 ####
densidad_est_parzen(data_buffalo, 80, 10)
density(data_buffalo, kernel = "rectangular", bw = 10/sqrt(3), from = 80, to = 80, n = 1)$y
hist(data_buffalo, freq = F, xlim = c(20, 140), breaks = seq(20, 140, 10))$density

## 12 ####
x_0 <- seq(25, 126.4, (126.4-25)/200)
parzen <- densidad_est_parzen(data_buffalo, x_0, 10)
par(mfrow=c(1,1))
plot(x_0, parzen)

## 13 ####
x_0 <- seq(25, 126.4, (126.4-25)/199)
par(mfrow=c(3,1))

# 10
hist(data_buffalo, freq = F, ylim = c(0, .022))
parzen_10 <- densidad_est_parzen(data_buffalo, x_0, 10)
lines(x_0, parzen_10, col = "red")

# 20
hist(data_buffalo, freq = F, ylim = c(0, .022))
parzen_20 <- densidad_est_parzen(data_buffalo, x_0, 20)
lines(x_0, parzen_20, col = "red")

# 50
hist(data_buffalo, freq = F, ylim = c(0, .022))
parzen_50 <- densidad_est_parzen(data_buffalo, x_0, 50)
lines(x_0, parzen_50, col = "red")

# Ejercicio 3 ####
data_ninos <- read.csv(here("aprendizaje_estadistico/Clase_4/data/datos_sim_ninos.csv"))
data_ninos

## 14 ####
par(mfrow=c(1,1))
hist(data_ninos$x, freq = F,
     main="h = 0.5",
     xlab="Peso de los ninios (Kg)",
     ylab="Density",
     ylim = c(0, 0.3))
dens <- density(data_ninos$x, kernel = "gaussian", bw = 0.5)
lines(dens$x, dens$y, col = "red")

## 15 ####
density(data_ninos$x, kernel = "gaussian", bw = 0.5, from = 16, to = 22, n = 4)$y

## 16 ####
par(mfrow=c(1,1))
hist(data_ninos$x, freq = F,
     main="h = 0.15 (azul), h = 0.5 (rojo), h = 10 (verde)",
     xlab="Peso de los ninios (Kg)",
     ylab="Density", ylim = c(0, 0.32))
dens <- density(data_ninos$x, kernel = "gaussian", bw = 0.15)
lines(dens$x, dens$y, ylim = c(0, 0.3), col = "blue")
dens <- density(data_ninos$x, kernel = "gaussian", bw = 0.5)
lines(dens$x, dens$y, ylim = c(0, 0.3), col = "red")
dens <- density(data_ninos$x, kernel = "gaussian", bw = 10)
lines(dens$x, dens$y, ylim = c(0, 0.3), col = "green")

## 18 ####
# hrot <- 1.06 * min(s, IQR/1.34) * n^(1/5)
hrot <- 1.06 * min(sd(data_ninos$x), IQR(data_ninos$x)/1.34) * (length(data_ninos$x))^(-1/5)
hrot

hsilv <- bw.ucv(data_ninos$x)
hsilv

par(mfrow=c(1,1))
hist(data_ninos$x, freq = F,
     main="h = hrot = 0.622 (azul), h = hsilv = 0.673 (rojo)",
     xlab="Peso de los ninios (Kg)",
     ylab="Density", ylim = c(0, 0.32))
dens <- density(data_ninos$x, kernel = "gaussian", bw = hrot)
lines(dens$x, dens$y, ylim = c(0, 0.3), col = "blue")
dens <- density(data_ninos$x, kernel = "gaussian", bw = hsilv)
lines(dens$x, dens$y, ylim = c(0, 0.3), col = "red")
