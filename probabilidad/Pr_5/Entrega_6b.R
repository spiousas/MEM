# Parte 1 ####
n <- 5
p <- 1/5 #*x
b_unif <- 4
y0 <- 2
x0 <- 3.6

# a ####
dbinom(x = y0, size = n, prob = x0*p)

# b ####
n*p*b_unif/2

# c ####
n*p*b_unif/2 - n*p^2*b_unif^2/3 + (n*p)^2*b_unif^2/12

# Parte 2 ####
a_unif <- -2.7
b_unif <- 2.7
b0 <- 1.2
b1 <- 4
sigma <- sqrt(0.01)
x0 <- 1

# a ####
Ex <- 1/2 * (a_unif+b_unif)
Ex

# b ####
b0 + b1 * x0

# c ####
b0 + b1 * Ex

# d ####
1/12 * (b_unif-a_unif)^2

# e ####
sigma^2

# f ####
sigma^2 + b1^2 * 1/12 * (b_unif-a_unif)^2

# Parte 3 ####
lambda_x <- 7
lambda_y <- 2
lambda_xy <- lambda_x + lambda_y
h <- 8
x0 <- 4

# a ####
dpois(x = h, lambda = lambda_xy)

# b ####
dpois(x = x0, lambda = lambda_x) * dpois(x = h - x0, lambda = lambda_y) / dpois(x = h, lambda = lambda_xy)

# c ####
h * lambda_x / (lambda_xy)

# Parte 4 ####
lambda <- 8
p <- 0.94
n0 <- 11
x0 <- 11

## a ####
n0 * p

## a ####
lambda * p

## c ####
dpois(x = x0, lambda = lambda * p)
