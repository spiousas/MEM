# Ejercicio 10 ####
rnormCont <- function(n, epsilon, mu1, sd1, mu2, sd2) {
 indic <- rbinom(size = n, prob = epsilon, n=1) 
 n2 <- sum(indic)
 n1 <- n-n2
 c(rnorm(n1, mu1, sd1), rnorm(n2, mu2, sd2))
}

sd(rnormCont(1000, 1, 0, 1, 0, 100))

epsilons <- c(.01, .05, 0.1, 0.2)
mu1 <- 0
mu2 <- 0
sd1 <- 1
sd2 <- 10
n <- 1000

var_means <- c()
var_medians <- c()
var_mean_trim <- c()

set.seed(123)
for (epsilon in epsilons) {
  mean_i <- c()
  median_i <- c()
  mean_trim_i <- c()
  for (i in 1:1E5) {
    sample_i <- rnormCont(n, epsilon, mu1, sd1, mu2, sd2)
    mean_i <- c(mean_i, mean(sample_i))
    median_i <- c(median_i, median(sample_i))
    mean_trim_i <- c(mean_trim_i, mean(sample_i, trim = 0.05))
  }
  var_means <- c(var_means, n * var(mean_i))
  var_medians <- c(var_medians, n * var(median_i))
  var_mean_trim <- c(var_mean_trim, n * var(mean_trim_i))
}
var_means
var_medians
var_mean_trim

tabla <- tibble(epsilon = epsilons,
                media = var_means,
                mediana = var_medians,
                media_podada = var_mean_trim)
tabla
