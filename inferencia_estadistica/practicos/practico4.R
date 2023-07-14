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

# Ejercicio 3 ####
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

# Ejercicio 4 ####

# a ####
alfa <- 0.05
n <- 10
m <- 16
Sa2 <- 3.2
Sb2 <- 7.5

cuantil_sup <- qf(df1 = n-1, df2 = m-1 , p = 1-alfa/2)
cuantil_sup
cuantil_inf <- qf(df1 = n-1, df2 = m-1 , p = alfa/2)
cuantil_inf

t <- Sa2/Sb2
t

alfa <- 0.01

cuantil_sup <- qf(df1 = n-1, df2 = m-1 , p = 1-alfa/2)
cuantil_sup
cuantil_inf <- qf(df1 = n-1, df2 = m-1 , p = alfa/2)
cuantil_inf

# Ejercicio 5 ####
testexp <- function(x, j, lambda0, alpha) {
  x_mean <- mean(x)
  n <- length(x)
  t <- 2*n* (lambda0*x_mean-1-log(lambda0*x_mean))
  cuantil <- qchisq(df = j, p = 1-alpha)
  test <- t > cuantil
  return(test)
}

lambda0 <- 1
j <- 1
alpha <- .05

Nrep <- 1e4
test_res <- rep(NA, Nrep)
for (i in 1:Nrep) {
  x <- rexp(n = 100, rate = lambda0)
  test_res[i] <- testexp(x = x, j = j, lambda0 = lambda0, alpha = alpha)
}
mean(test_res)

# Ejercicio 6 ####
alfa <- .05
cuantil <- qnorm(1-alfa)
cuantil
n <- 32
m <- 32

x_mean <- 10/n
y_mean <- 20/n

t <- abs(x_mean-y_mean)/sqrt((x_mean*(1-x_mean)+y_mean*(1-y_mean))/32)
t
