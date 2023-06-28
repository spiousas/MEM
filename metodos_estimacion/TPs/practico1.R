library(tidyverse)

# Ejercicio 4 ####
Nrep <- 10000
n <- 1000
p <- 0.5

phat <- rep(NA, Nrep)
S2 <- rep(NA, Nrep)
for (i in 1:Nrep) {
  x <- rbinom(size = 1, n = n, p = p)
  phat[i] <- mean(x)
  S2[i] <- var(x)
}

hist(phat, freq = FALSE)
x <- seq(0.3, 0.8, .001)
y <- dnorm(x=x, mean = p, sd = sqrt(mean(S2)/n))
lines(x,y)
