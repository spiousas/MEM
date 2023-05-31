data <- c(0.96, 0.97, 1.12, 1.16, 1.03, 0.95, 0.91, 0.87, 0.96, 1.04,
          0.77, 0.99, 0.84, 1.08, 1.12, 0.78, 0.95, 0.93, 1.09, 0.92,
          1.00, 0.92, 1.02, 0.90, 0.87, 0.85, 1.03, 1.04, 0.92, 1.07)
mu <- sum(data)/length(data)
mu
sigma <- sum(data^2)/length(data) -  (sum(data)/length(data))^2
sigma
var(data) - sigma

# Ejercicio 6 ####
est1_unif <- function(x) {
  2*mean(x)
}

est2_unif <- function(x) {
  max(x)
}

# Ejercicio 7 ####
## a ####
histograma_est1_unif <- function(theta, n, Nrep, seed) {
  set.seed(seed)
  est_1 <- c()
  for (i in 1:Nrep) {
    est_1[i] <- est1_unif(runif(n = n, min = 0, max = theta))
  }
  hist(est_1)
}

histograma_est1_unif(3, 100, 1000, 1)

## b ####
histograma_est2_unif <- function(theta, n, Nrep, seed) {
  set.seed(seed)
  est_2 <- c()
  for (i in 1:Nrep) {
    est_2[i] <- est2_unif(runif(n = n, min = 0, max = theta))
  }
  hist(est_2)
}

histograma_est2_unif(3, 20, 100, 1)

## c ####

histograma_est1_unif(3, 5, 1000, 0787)
histograma_est1_unif(3, 30, 1000, 0787)
histograma_est1_unif(3, 50, 1000, 0787)
histograma_est1_unif(3, 100, 1000, 0787)
histograma_est1_unif(3, 1000, 1000, 0787)

histograma_est2_unif(3, 5, 1000, 0787)
histograma_est2_unif(3, 30, 1000, 0787)
histograma_est2_unif(3, 50, 1000, 0787)
histograma_est2_unif(3, 100, 1000, 0787)
histograma_est2_unif(3, 1000, 1000, 0787)

# d ####
densidad_est1_unif <- function(theta, n, Nrep, seed) {
  set.seed(seed)
  est_1 <- c()
  for (i in 1:Nrep) {
    est_1[i] <- est1_unif(runif(n = n, min = 0, max = theta))
  }
  density(est_1)
}

plot(densidad_est1_unif(3, 100, 1000, 1))

densidad_est2_unif <- function(theta, n, Nrep, seed) {
  set.seed(seed)
  est_2 <- c()
  for (i in 1:Nrep) {
    est_2[i] <- est2_unif(runif(n = n, min = 0, max = theta))
  }
  density(est_2)
}

plot(densidad_est2_unif(3, 20, 100, 1))

# e ####
ECME_est1_unif <- function(theta, n, Nrep, seed) {
  set.seed(seed)
  est_1 <- c()
  for (i in 1:Nrep) {
    est_1[i] <- est1_unif(runif(n = n, min = 0, max = theta))
  }
  1/Nrep * sum((est_1-theta)^2)
}

ECME_est1_unif(3, 5, 1000, 0787)
ECME_est1_unif(3, 30, 1000, 0787)
ECME_est1_unif(3, 50, 1000, 0787)
ECME_est1_unif(3, 100, 1000, 0787)
ECME_est1_unif(3, 1000, 1000, 0787)

ECME_est2_unif <- function(theta, n, Nrep, seed) {
  set.seed(seed)
  est_2 <- c()
  for (i in 1:Nrep) {
    est_2[i] <- est2_unif(runif(n = n, min = 0, max = theta))
  }
  1/Nrep * sum((est_2-theta)^2)
}

ECME_est2_unif(3, 5, 1000, 0787)
ECME_est2_unif(3, 30, 1000, 0787)
ECME_est2_unif(3, 50, 1000, 0787)
ECME_est2_unif(3, 100, 1000, 0787)
ECME_est2_unif(3, 1000, 1000, 0787)

