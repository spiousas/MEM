# Ejercicio 5 ####

p <- .5
x <- sum(rbinom(n = 2,size = 1, prob = p))

test <- c()
Nrep <- 10000
for (i in 1:Nrep) {
  x <- sum(rbinom(n = 2,size = 1, prob = p))
  test[i] <- x!=1
}
mean(test)

# Con binomial
for (i in 1:Nrep) {
  x <- rbinom(n = 2,size = 2, prob = p)
  test[i] <- x!=1
}
mean(test)

thetas <- 0:4
pot_emp <- rep(NA, length(thetas))
test <- rep(NA, Nrep)

for (i in 1:length(thetas)) {
  for (j in 1:Nrep) {
    x <- rbinom(n = 2,size = 2, prob = thetas[i]/4)
    test[j] <- x!=1
  }
  pot_emp[i] <- mean(test)
}
pot_emp

# Con hipergeométrica
1-choose(2,1)*choose(2,1)/choose(4,2)

pot_teo <- function(theta) {
  1-choose(theta,1)*choose(4-theta,1)/choose(4,2)
}
pot_teo(thetas)

# Ejercicio 7 ####
data <- c(37, 39.5, 41.7, 42, 40, 41.25, 43, 44.05, 38, 38.5)
sum(data)
n <- length(data)
sd2_0 <- 25 # La varianza es dato

mu1 <- 37
mu2 <- 40

## a ####
t <- (sum(data) - mu1*n)/sqrt(sd2_0*n)
t

sqrt(10) * (mean(data)-37)/5
qnorm(p = .95)

## b ####
1-pnorm(t)

## c ####
H <- sqrt(sd2_0*n) * qnorm(p = .95) + mu1*n
(H - n*mu2)/sqrt(sd2_0*n)

pnorm(q = (H - n*mu2)/sqrt(sd2_0*n))

## d ####

# Quiero ver la pinta de la potencia
ns <- 10:100
EII <- pnorm(qnorm(p = .95) + ((37-40)*ns)/sqrt(25*ns))
pot <- 1 - EII

plot(ns, EII)

# Pareciera ser menor a 0.05 a partir de n~30

# La cuenta
((qnorm(p = .05) - qnorm(p = .95)) * (-5/3))^2

# Ejercicio 8 ####
# Miremos que forma tiene la potencia para distintos mues verdaderos

mu1 <- 200
sd0 <- sqrt(25)
n <- 10
mus <- seq(190, 210, 1)

pot <- 1 - pnorm(q = qnorm(.95) + sqrt(n) * (mu1-mus)/sd0)
plot(mus, pot)

EII <- pnorm(q = qnorm(.95) + sqrt(n) * (mu1-210)/sd0)
EII

# Ejercicio 10 ####
sigmas <- seq(0, 10, .1)
C <- -1/(2*sigmas^2)
plot(sigmas, C)

# Ejercicio 11 ####
## b-i ####
qchisq(p = .01, df = 21)
21*0.6

## b-ii ####
pchisq(df = 21, q = 21*0.6)

## b-iii ####
1-pchisq(df = 21, q = 1/0.8 * qchisq(p = .01, df = 21))

# Ejercicio 13 ####
## a ####
# Quiero ver la potencia
thetas <- seq(.9,1.1,.001)
alpha <- .05
n <- 100
pot <- 1 - (1-alpha)/(thetas)^(n)
plot(thetas, pot)
pot[thetas==1]
log(9.5)/log(1.1)

## b-i ####
p0 <- .4
n <- 100
alfa <- .05

a <- n*p0
b <- sqrt(n*p0*(1-p0))
(87-a)/b 
(87-a)/b > qnorm(1-alfa)

## b-ii ####
1-pnorm((87-a)/b )

## b-iii ####
qnorm(1-alfa) * b + a

p2 <- .45
a2 <- n*p2
b2 <- sqrt(n*p2*(1-p2))

q <- ((qnorm(1-alfa) * b + a) - a2)/b2

1 - pnorm(q)

# Ejercicio 14 ####
## a ####

xprom <- 21
n <- 45
alfa <- 0.01
lambda0 <- 20

t <- sqrt(n) * (xprom - lambda0)/sqrt(lambda0)
t
t > qnorm(1-alfa)

#p-valor
1-pnorm(t)

## b ####
lambda1 <- 22
pnorm((qnorm(1-alfa)*sqrt(n*lambda0)+n*lambda0-n*lambda1)/sqrt(n*lambda1))

## c ####
EII <- 0.05

n_obj <- ((qnorm(EII)*sqrt(lambda1) - qnorm(1-alfa)*sqrt(lambda0))/(lambda0-lambda1))^2
n_obj
n_obj <- ceiling(n_obj)
n_obj
pnorm((qnorm(1-alfa)*sqrt(n_obj*lambda0)+n_obj*lambda0-n_obj*lambda1)/sqrt(n_obj*lambda1))

# Ejercicio 15 ####
sumX <- 43
n <- 100
p0 <- 0.5
alfa <- 0.05

## a ####
t <- (sumX - n*p0)/sqrt(n*p0*(1-p0))
t
t < qnorm(alfa)

# El p-valor
pnorm(t)

## b ####
p1 <- .55
1 - pnorm((qnorm(alfa) * sqrt(n*p0*(1-p0)) + n*(p0-p1))/sqrt(n*p1*(1-p1)))

# Ejercicio 16 ####
lambda0 <- 1/4
n <- 30
alfa <- 0.05

## a ####
n/lambda0
sqrt(n/lambda0^2)
qnorm(1-alfa)

data <- c(3, 4, 5, 3, 2, 7, 6, 4, 6, 4, 7, 2, 3, 4, 6, 7, 3, 5, 6, 6, 4, 5, 5, 7, 3, 2, 2, 4, 3, 5)

t <- (sum(data)-n/lambda0) / sqrt(n/lambda0^2)
t

1-pnorm(t)

## b ####
qnorm(1-alfa) * sqrt(n/lambda0^2) + n/lambda0

lambdas <- seq(.01,1,.01)
pot <- 1-pnorm((qnorm(1-alfa) * sqrt(n/lambda0^2) + n/lambda0 - n/lambdas)/sqrt(n/lambdas^2))
plot(lambdas, pot)
pot[lambdas==0.25]

## c ####
lambda1 <- 1/5
n_obj<- ((qnorm(0.01)*1/lambda1 - qnorm(1-alfa)*1/lambda0)/(1/lambda0-1/lambda1))^2
n_obj 
ceiling(n_obj)

pot <- 1-pnorm((qnorm(1-alfa) * sqrt(n_obj/lambda0^2) + n_obj/lambda0 - n_obj/lambda1)/sqrt(n_obj/lambda1^2))
pot
