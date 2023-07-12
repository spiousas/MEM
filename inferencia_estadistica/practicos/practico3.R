# Ejercicio 1 ####

## a ####
x <- c(6.90, 7.10, 7.20, 7.07, 7.15, 7.04, 7.18, 6.95)
mu0 <- 7
sigma0 <- 0.15
n <- length(x)
alfa <- 0.05

t <- sqrt(n) * abs(mean(x)-mu0)/sigma0
t
cuantil <- qnorm(1-alfa/2)
cuantil
t>cuantil

## b ####
# Miremos la potencia

mus <- seq(6.5, 7.5, .01)
pot <- function(mu0, sigma0, n, alfa, mu) {
  1 - (pnorm(qnorm(1-alfa/2)+sqrt(n) * (mu0-mu)/sigma0) - pnorm(qnorm(alfa/2)+sqrt(n) * (mu0-mu)/sigma0))
}
plot(mus, pot(mu0 = 7, sigma0 = 0.15, n = 8, alfa = .05, mu = mus))

# El error de tipo II es 1-potencia
1 - pot(mu0 = 7, sigma0 = 0.15, n = 8, alfa = .05, mu = 7.1)

## c ####
pot(mu0 = 7, sigma0 = 0.15, n = 8, alfa = .05, mu = 7.1)
pot(mu0 = 7, sigma0 = 0.15, n = 8, alfa = .05, mu = 6.9)
# Como es de esperarse, la potencia es simétrica.
# El test no cumple con la potencia deseada

# Voy a buscar el mínimo con r para encontrar el n mínimo
target <- function(n) {
  abs(pot(mu0 = 7, sigma0 = .15, n = n, alfa = .05, mu = 7.1) - .8)
}

ceiling(optim(par = c(10), target )$par)

## Ejercicio 3 ####
## a ####

mu0 <- 0.7
x_media <- 0.85
S <- 0.05
n <- 8
alfa <- .05

t <- sqrt(n) * abs(x_media-mu0)/S
t
cuantil <- qt(df = n-1, p = 1-alfa/2)
cuantil
t > cuantil

## b ####
sigma0 <- 0.03
t <- (n-1) * (S/sigma0)^2
cuantil <- qchisq(df = 12, p = .05)
cuantil

pchisq(df = 12, q = t)

## Ejercicio 5 ####
n <- 3*60*60
x_mean <- 5029/n
alfa <- .01
cuantil <- qchisq(df = 1, p = 1-alfa)
cuantil
lambda0 <- .5

t <- 2*n * (lambda0 - x_mean - x_mean * log(lambda0/x_mean))
t
t > cuantil

p_valor <- 1 - pchisq(df = 1, q = t)
p_valor
