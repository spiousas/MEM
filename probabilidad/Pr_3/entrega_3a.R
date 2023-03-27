# a ####
n <- 65
mu <- 3.72
Var <- .19
sigma <- sqrt(Var)

1 - pnorm((240-n*mu)/(sqrt(n)*sigma))

# b ####
peso <- 3217
p <- 0.97

qnorm(1-p)
sigma*qnorm(1-p)

# resolvente
a <- 3.72
b <- -0.82
c <- -3217
w1 <- (-b + sqrt((b)^2-4*a*c))/(2*a)
w2 <- (-b - sqrt((b)^2-4*a*c))/(2*a)
w1^2

# c ####
mu <- 0
sigma <- sqrt(3)
p <- 0.93

(0.2-mu)/sigma

(qnorm((1-p)/2) / (-(0.2-mu)/sigma))^2

