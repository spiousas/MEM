# Estimadores ####
est1_unif <- function(x) {
  2*mean(x)
}

est2_unif <- function(x) {
  max(x)
}

# Histogramas ####
histograma_est1_unif <- function(theta, n, Nrep, seed) {
  set.seed(seed)
  est_1 <- c()
  for (i in 1:Nrep) {
    est_1[i] <- est1_unif(runif(n = n, min = 0, max = theta))
  }
  hist(est_1)
}

histograma_est1_unif(3, 100, 1000, 1)

histograma_est2_unif <- function(theta, n, Nrep, seed) {
  set.seed(seed)
  est_2 <- c()
  for (i in 1:Nrep) {
    est_2[i] <- est2_unif(runif(n = n, min = 0, max = theta))
  }
  hist(est_2)
}

histograma_est2_unif(3, 20, 100, 1)