# Ejercicio 1 ####
x <- c(5.1, 5.2, 5.6, 5.1, 5.5, 5.8, 5.9, 4.9, 5.2, 5.6)

## a ####
beta <- 0.05
var <- 1/4
mean(x)+qnorm(beta/2)*sqrt(var/length(x))
mean(x)+qnorm(1-beta/2)*sqrt(var/length(x))

## b ####
(2*19.6)^2*var

# Ejercicio 2 ####
intervalo_mu_exacto <- function(nivel, desvio, x) {
  ci_low <- mean(x)+qnorm(nivel/2)*desvio*sqrt(1/length(x))
  ci_up <- mean(x)+qnorm(1-nivel/2)*desvio*sqrt(1/length(x))
  c(ci_low, ci_up)
}

intervalo_mu_exacto(nivel = beta, 
                    desvio = sqrt(var), 
                    x = x)

# Ejercicio 3 ####
## a ####
cubrimiento_empirico_mu <- function(mu, sigma, nivel, n, Nrep, seed) {
  set.seed(seed)
  cubrimiento <- rep(NA, Nrep)
  for (i in 1:Nrep) {
    x <- rnorm(n = n, mean = mu, sd = sigma)
    ci <- intervalo_mu_exacto(nivel = nivel, 
                              desvio = sigma, 
                              x = x)
    cubrimiento[i] <- (mu>ci[1]) & (mu<ci[2])
  }
  mean(cubrimiento)
}

cubrimiento_empirico_mu(mu = 5, 
                        sigma = 0.2, 
                        nivel = 0.05, 
                        n = 100, 
                        Nrep = 100000, 
                        seed = 1)

# Ejercicio 6 ####
intervalo_mu_exacto <- function(nivel, desvio, x) {
  n <- length(x)
  if (is.null(desvio)) {
    ci_low <- mean(x)+qt(df = n-1, p = nivel/2)*sd(x)*sqrt(1/n)
    ci_up <- mean(x)+qt(df = n-1, p = 1-nivel/2)*sd(x)*sqrt(1/n)
  } else {
    ci_low <- mean(x)+qnorm(p = nivel/2)*desvio*sqrt(1/n)
    ci_up <- mean(x)+qnorm(p = 1-nivel/2)*desvio*sqrt(1/n)
  }
  c(ci_low, ci_up)
}

intervalo_mu_exacto(nivel = beta, 
                    desvio = sqrt(var), 
                    x = x)

intervalo_mu_exacto(nivel = beta, 
                    desvio = NULL, 
                    x = x)

# Ejercicio 7 ####
cubrimiento_empirico_mu <- function(mu, sigma, nivel, n, Nrep, seed) {
  set.seed(seed)
  cubrimiento <- rep(NA, Nrep)
  for (i in 1:Nrep) {
    x <- rnorm(n = n, mean = mu, sd = sigma)
    ci <- intervalo_mu_exacto(nivel = nivel, 
                              desvio = NULL, 
                              x = x)
    cubrimiento[i] <- (mu>ci[1]) & (mu<ci[2])
  }
  mean(cubrimiento)
}
cubrimiento_empirico_mu(mu = 5, 
                        sigma = 0.2, 
                        nivel = 0.05, 
                        n = 100, 
                        Nrep = 10000, 
                        seed = 5)

# Ejercicio 9 ####
x <- c(1.52, 1.65, 1.72, 1.65, 1.72, 1.83, 1.62, 1.75, 1.72, 1.68, 1.51, 1.65, 1.58,
       1.65, 1.61, 1.70, 1.60, 1.73, 1.61, 1.52, 1.81, 1.72, 1.50, 1.82, 1.65)

## a ####
intervalo_mu_exacto(nivel = 0.05, 
                    desvio = NULL, 
                    x = x)

## b ####
intervalo_sd2_exacto <- function(nivel, x) {
  n <- length(x)
  ci_low <- (n-1)*sd(x)^2/(qchisq(p = 1-nivel/2, df = n-1))
  ci_up <- (n-1)*sd(x)^2/(qchisq(p = nivel/2, df = n-1))
  c(ci_low, ci_up)
}

intervalo_sd2_exacto(nivel = 0.1, x = x)

cubrimiento_empirico_sd2 <- function(sigma, nivel, n, Nrep, seed) {
  set.seed(seed)
  cubrimiento <- rep(NA, Nrep)
  for (i in 1:Nrep) {
    x <- rnorm(n = n, mean = 0, sd = sigma)
    ci <- intervalo_sd2_exacto(nivel = nivel, 
                               x = x)
    cubrimiento[i] <- (sigma>sqrt(ci[1])) & (sigma<sqrt(ci[2]))
  }
  mean(cubrimiento)
}

cubrimiento_empirico_sd2(sigma = 0.2, 
                         nivel = 0.05, 
                         n = 100, 
                         Nrep = 10000, 
                         seed = 1)

