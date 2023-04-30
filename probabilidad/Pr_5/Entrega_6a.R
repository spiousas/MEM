# Parte 1 ####
c <- 0.213
x_cond <- 0.4
y_max <- 2.5
y_lim <- 0.75
  
## fx ####
c*y_max^2/2
c/2
sqrt(y_max)

## fy ####
2*c/3

## a ####
2*c/3* 2/7 * (y_lim)^(7/2)

## b ####
fxy <- c*x_cond^2 
fx <- c*y_max^2/2*0.4^2 - c/2*0.4^6
a <- fxy/fx

a/2*(y_lim^2-(x_cond^2)^2)

## c ####
a/3*(y_max^3-(x_cond^2)^3)

## d ####
2*c/3* 2/9 * y_max^(9/2)

# Parte 2 ####
lambda <- 3.79
x_0 <- 0.26
y_0 <- 0.79

## a ####
exp(-lambda*x_0)

## b ####
exp(-lambda*(y_0-x_0))

## c ####
1/lambda + x_0

## d ####
1/lambda^2

## e ####
1/sqrt(2)
