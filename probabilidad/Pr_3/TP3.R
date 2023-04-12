# Ejercicio 1 ####
## a ####
Var <- 0.25
sigma <- sqrt(Var)
n <- 100

0.1*sqrt(n)/sqrt(sigma)
1-2*pnorm(-0.1*sqrt(n)/sqrt(sigma))

## b ####
p <- 0.99

(qnorm((p-1)/(-2))/(-0.1 / sigma))^2

# Ejercicio 2 ####
## a ####
lambda <- 2
n <- 62
mu <- 1/lambda
Var <- 1/(lambda)^2
sigma <- sqrt(Var)

1-pnorm((30-n*mu)/(sqrt(n*Var)))

(30-n*mu)/(sqrt(n*Var))

## b ####
p <- 0.99

qnorm(1-p) * 1/4

# resolvente
a <- 0.5
b <- -0.58
c <- -30
w1 <- (-b + sqrt((b)^2-4*a*c))/(2*a)
w2 <- (-b - sqrt((b)^2-4*a*c))/(2*a)
w1^2

# Ejercicio 3 ####
## a ####
mu_A <- (15^3-11^3)/(3*52)
mu_A
V_A <- 1/52^2 * (15^3 - 2044/3 * 15^2 + 2044^2/9 * 15 - 11^3 + 2044/3 * 11^2 - 2044^2/9 * 11)
V_A <- ((15^4) - (11^4))/(4*52) - (2044^2/(3*52)^2)
sigma_A <- sqrt(V_A)
sigma_A

## b ####
mu_B <- 18
sigma_b <- 1.3
V_B <- sigma_b^2

mu_C <- mu_A + mu_B 
mu_C

V_C <- V_A + V_B
V_C
sigma_C <- sqrt(V_C)
sigma_C

## b ####
n <- 256

mu_Q <- n*mu_C
mu_Q

V_Q <- n*V_C
V_Q

sigma_Q <- sqrt(V_Q)
sigma_Q 

lim_inf <- (7950-mu_Q)/sigma_Q
lim_inf
lim_sup <- (8000-mu_Q)/sigma_Q
lim_sup

pnorm(lim_sup)-pnorm(lim_inf)

# Ejercicio 4 ####
## a ####
mu_H <- 4.6
V_H <- .49
sigma_H <- sqrt(V_H)

n <- 64

mu_T <- n * mu_H
mu_T

sigma_T <- sqrt(n) * sigma_H
sigma_T

1 - pnorm((290-mu_T)/sigma_T)

## b ####
peso <- 3375
p <- 0.99

qnorm(1-p)
sigma*qnorm(1-p)

# resolvente
a <- -mu_H
b <- -qnorm(1-p)*sigma_H
c <- peso
w1 <- (-b + sqrt((b)^2-4*a*c))/(2*a)
w2 <- (-b - sqrt((b)^2-4*a*c))/(2*a)
w1
w2

n <- ceiling(w1^2)

# Comprobación
1-pnorm((peso-n*mu_H)/(sqrt(n)*sigma_H))

# Ejercicio 9 ####
## a ####
mu_obs <- 72.3
patron <- 70
sigma <- 10
n <- 4

2 * pnorm(-abs(mu_obs-patron)*sqrt(n)/sigma)

## b ####
mu_obs <- 72.3
patron <- 70
sigma <- 10
n <- 40

2 * pnorm(-abs(mu_obs-patron)*sqrt(n)/sigma)
