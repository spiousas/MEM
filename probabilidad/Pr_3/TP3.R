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

