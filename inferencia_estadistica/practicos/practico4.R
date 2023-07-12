# Ejercicio 1 ####

## a ####
x <- c(25, 19.5, 16.6, 21.3, 20.7, 16.8)
y <- c(23.8, 19, 15.9, 20.4, 19.6, 15.8)
z <- x-y
n <- length(z)
alfa <- 0.05

shapiro.test(z)

t <- sqrt(n) * abs(mean(z))/sd(z)
t
cuantil <- qt(df = n-1, p = 1-alfa/2)
cuantil

## b ####
mean(z) - cuantil * sd(z)/sqrt(n)
mean(z) + cuantil * sd(z)/sqrt(n)

# Ejercicio 2 ####
x <- c(255, 481, 360, 368, 425, 283, 311, 368)
y <- c(226, 425, 311, 311, 255, 340, 311, 284)
z <- x-y
n <- length(z)
alfa <- 0.01

shapiro.test(z)
hist(z)

## a ####
mean(z)
sd(z)
t <- sqrt(n) * abs(mean(z))/sd(z)
t
cuantil <- qt(df = n-1, p = 1-alfa/2)
cuantil

## b ####
2*(1-pt(df = n-1, q = t))

2 * pt(df = n-1, q = -t)

# Ejercicio 4 ####
alfa = 0.05
cuantil <- qt(df = 18, p = 1-alfa/2)
cuantil

## b-i ####
x_mean <- 130
y_mean <- 120
Sx <- 10
Sy <- 8

t <- abs(x_mean-y_mean)/sqrt(Sx^2/10+Sy^2/10)
t

## b-ii ####
p_valor <- 2*(1-pt(df = 18, q = t))
p_valor
