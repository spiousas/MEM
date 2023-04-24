library(tidyverse)
library(here)

# Ejercicio 1 ####
## 1 ####
set.seed(1)
data <- runif(n = 25, min = 0, max = 1)

## 2 ####
pdf(file = here("aprendizaje_estadistico/Pr_4/figs/Ej2.pdf"), 
    width = 8, 
    height = 8)

par(mfrow=c(1,1))
plot(ecdf(data),
     main="Ej. 2 - Unif[0,1], n=25, seed=1")
x <- seq(0,1,0.01)
lines(x, x, col = "red")

dev.off()

# #3 ####
x <- seq(0,1,0.01)
N <- 25

pdf(file = here("aprendizaje_estadistico/Pr_4/figs/Ej3.pdf"),  
    width = 8,
    height = 8)

par(mfrow=c(2,2))

set.seed(1)
data <- runif(n = N, min = 0, max = 1)
plot(ecdf(data),
     main="Ej. 3 - Unif[0,1], n=25, seed=1")
lines(x, x, col = "red")

set.seed(2)
data <- runif(n = N, min = 0, max = 1)
plot(ecdf(data),
     main="Ej. 3 - Unif[0,1], n=25, seed=2")
lines(x, x, col = "red")

set.seed(3)
data <- runif(n = N, min = 0, max = 1)
plot(ecdf(data),
     main="Ej. 3 - Unif[0,1], n=25, seed=3")
lines(x, x, col = "red")

set.seed(4)
data <- runif(n = N, min = 0, max = 1)
plot(ecdf(data),
     main="Ej. 3 - Unif[0,1], n=25, seed=4")
lines(x, x, col = "red")

dev.off()

## 4 ####
set.seed(123)
x <- seq(0,1,0.01)
N <- 1000

pdf(file = here("aprendizaje_estadistico/Pr_4/figs/Ej4.pdf"),  
    width = 8,
    height = 8)

par(mfrow=c(2,2))

set.seed(1)
data <- runif(n = N, min = 0, max = 1)
plot(ecdf(data),
     main="Ej. 4 - Unif[0,1], n=1000, seed=1")
lines(x, x, col = "red")

set.seed(2)
data <- runif(n = N, min = 0, max = 1)
plot(ecdf(data),
     main="Ej. 4 - Unif[0,1], n=1000, seed=2")
lines(x, x, col = "red")

set.seed(3)
data <- runif(n = N, min = 0, max = 1)
plot(ecdf(data),
     main="Ej. 4 - Unif[0,1], n=1000, seed=3")
lines(x, x, col = "red")

set.seed(4)
data <- runif(n = N, min = 0, max = 1)
plot(ecdf(data),
     main="Ej. 4 - Unif[0,1], n=1000, seed=4")
lines(x, x, col = "red")

dev.off()

## 5 ####
set.seed(1)
data <- rexp(n = 25, rate = 1)

pdf(file = here("aprendizaje_estadistico/Pr_4/figs/Ej5_1.pdf"),  
    width = 8,
    height = 8)

par(mfrow=c(1,1))
plot(ecdf(data),
     main="Ej. 5 - Exp(1), n=25, seed=1")
x <- seq(0,5,0.01)
lines(x, 1-exp(-x), col = "blue")

dev.off()

# n=25 ####
x <- seq(0,6,0.01)
N <- 25

pdf(file = here("aprendizaje_estadistico/Pr_4/figs/Ej5_2.pdf"),  
    width = 8,
    height = 8)

par(mfrow=c(2,2))

set.seed(1)
data <- rexp(n = N, rate = 1)
plot(ecdf(data),
     main="Ej. 5 - Exp(1), n=25, seed=1",
     xlim = c(0,6))
lines(x, 1-exp(-x), col = "blue")

set.seed(2)
data <- rexp(n = N, rate = 1)
plot(ecdf(data),
     main="Ej. 5 - Exp(1), n=25, seed=2",
     xlim = c(0,6))
lines(x, 1-exp(-x), col = "blue")

set.seed(3)
data <- rexp(n = N, rate = 1)
plot(ecdf(data),
     main="Ej. 5 - Exp(1), n=25, seed=3",
     xlim = c(0,6))
lines(x, 1-exp(-x), col = "blue")

set.seed(4)
data <- rexp(n = N, rate = 1)
plot(ecdf(data),
     main="Ej. 5 - Exp(1), n=25, seed=4",
     xlim = c(0,6))
lines(x, 1-exp(-x), col = "blue")

dev.off()

# n=1000 ####
x <- seq(0,8,0.01)
N <- 1000

pdf(file = here("aprendizaje_estadistico/Pr_4/figs/Ej5_3.pdf"),  
    width = 8,
    height = 8)

par(mfrow=c(2,2))

set.seed(1)
data <- rexp(n = N, rate = 1)
plot(ecdf(data),
     main="Ej. 5 - Exp(1), n=1000, seed=1",
     xlim = c(0,8))
lines(x, 1-exp(-x), col = "blue")

set.seed(2)
data <- rexp(n = N, rate = 1)
plot(ecdf(data),
     main="Ej. 5 - Exp(1), n=1000, seed=2",
     xlim = c(0,8))
lines(x, 1-exp(-x), col = "blue")

set.seed(3)
data <- rexp(n = N, rate = 1)
plot(ecdf(data),
     main="Ej. 5 - Exp(1), n=1000, seed=3",
     xlim = c(0,8))
lines(x, 1-exp(-x), col = "blue")

set.seed(4)
data <- rexp(n = N, rate = 1)
plot(ecdf(data),
     main="Ej. 5 - Exp(1), n=1000, seed=4",
     xlim = c(0,8))
lines(x, 1-exp(-x), col = "blue")

dev.off()

## 6 ####
x <- seq(0,1,0.01)

pdf(file = here("aprendizaje_estadistico/Pr_4/figs/Ej6.pdf"),  
    width = 8,
    height = 8)

par(mfrow=c(1,1))
plot(x, x, type='n',
     main="Ej. 6 - Unif[0,1] (azul) y Exp(1) (rojo)",
     xlab="x",
     ylab="f(x)")
lines(x, rep(1, length(x)), col = "red")
lines(x, exp(-x), col = "blue")

dev.off()

# Ejercicio 2 ####
data_buffalo <- scan(here("aprendizaje_estadistico/Clase_4/data/buffalo.txt"))
data_buffalo

## 1 ####
densidad_est_parzen <- function(x, x_0, h) {
  parzen <- 0*x_0
  for (i in 1:length(x_0)) {
    parzen[i] <- sum(data_buffalo<= x_0[i]+h & data_buffalo>= x_0[i]-h) / (length(data_buffalo)*2*h)
  }  
  parzen
}

density(data_buffalo, kernel = "rectangular", from = 80, to = 80, n = 1, bw = 10/sqrt(3))$y
densidad_est_parzen(data_buffalo, 80, 10)

## 2 ####
plot(density(data_buffalo, kernel = "rectangular", bw = 10/sqrt(3)))

## 3 ####
density(data_buffalo, kernel = "gaussian", from = 80, to = 80, n = 1, bw = 10)$y
plot(density(data_buffalo, kernel = "gaussian", bw = 10))
  
## 4 ####
par(mfrow=c(1,1))
hist(data_buffalo, freq = F,
     main="h=5 - Rectangular (azul), gaussian (rojo), Epanechnikov (verde)",
     xlab="Nieve caída",
     ylab="Density", ylim = c(0, 0.02))
dens <- density(data_buffalo, kernel = "rectangular", bw = 5)
lines(dens$x, dens$y, ylim = c(0, 0.02), col = "blue")
dens <- density(data_buffalo, kernel = "gaussian", bw = 5)
lines(dens$x, dens$y, ylim = c(0, 0.02), col = "red")
dens <- density(data_buffalo, kernel = "epanechnikov", bw = 5)
lines(dens$x, dens$y, ylim = c(0, 0.02), col = "green")

## 5 ####
bw.nrd(data_buffalo)

x <- c()
y <- c()
for (i in 1:length(data_buffalo)) {
  x[i] <- data_buffalo[i]
  y[i] <- density(data_buffalo, kernel = "gaussian", from = data_buffalo[i], to = data_buffalo[i], n =1, bw = bw.nrd(data_buffalo))$y
}

plot(x, y, col = "red")
lines(density(data_buffalo, kernel = "gaussian", bw = bw.nrd(data_buffalo)))

## 6 ####
par(mfrow=c(1,1))
hist(data_buffalo, freq = F,
     main="fh(-i) - i=17 (azul), i=20 (rojo), i=51 (verde)",
     xlab="Nieve caída",
     ylab="Density", ylim = c(0, 0.02))
dens <- density(data_buffalo[-17], kernel = "epanechnikov", bw = 5)
lines(dens$x, dens$y, ylim = c(0, 0.02), col = "blue")
dens <- density(data_buffalo[-20], kernel = "epanechnikov", bw = 5)
lines(dens$x, dens$y, ylim = c(0, 0.02), col = "red")
dens <- density(data_buffalo[-51], kernel = "epanechnikov", bw = 5)
lines(dens$x, dens$y, ylim = c(0, 0.02), col = "green")

## 7 ####
bw.ucv(data_buffalo)

## 8 ####
par(mfrow=c(1,1))
hist(data_buffalo, freq = F,
     main="Gaussian - h=Silverman (azul), h=ucv (rojo)",
     xlab="Nieve caída",
     ylab="Density", ylim = c(0, 0.02))
dens <- density(data_buffalo, kernel = "gaussian", bw = bw.nrd(data_buffalo))
lines(dens$x, dens$y, ylim = c(0, 0.02), col = "blue")
dens <- density(data_buffalo, kernel = "gaussian", bw = bw.ucv(data_buffalo))
lines(dens$x, dens$y, ylim = c(0, 0.02), col = "red")