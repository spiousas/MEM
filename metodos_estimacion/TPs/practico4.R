# Setup ####
pacman::p_load(tidyverse)

# Ejercicio 1 ####
# Una muestra con sigma conocido
x <- c(5.1, 5.2, 5.6, 5.1, 5.5, 5.8, 5.9, 4.9, 5.2, 5.6)

## a ####
beta <- 0.05
var <- 1/4
# Tambi'en puede ser mean(x)+qnorm(beta/2)*sqrt(var/length(x))
mean(x)-qnorm(1-beta/2)*sqrt(var/length(x))
mean(x)+qnorm(1-beta/2)*sqrt(var/length(x))

## b ####
# Es de los pocos casos en los que la puedo calcular y es n >= (2*qnorm(1-beta/2)/l)^2 * sigma^2
lmax <- 0.1
ceiling( (2*qnorm(1-beta/2)/lmax)^2 * var )

# Ejercicio 2 ####
intervalo_mu_exacto <- function(nivel, desvio, x) {
  alfa <- 1-nivel
  ci_low <- mean(x)-qnorm(1-alfa/2)*desvio*sqrt(1/length(x))
  ci_up <- mean(x)+qnorm(1-alfa/2)*desvio*sqrt(1/length(x))
  c(ci_low, ci_up)
}

intervalo_mu_exacto(nivel = 1-beta, 
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
                        nivel = 0.95, 
                        n = 100, 
                        Nrep = 100000, 
                        seed = 1)

## b ####
cubrimientos <- expand_grid(n = c(5, 10, 30, 50),
                            sigma = c(0.1, 1, 10),
                            Nrep = 1000, 
                            nivel = 0.95,
                            mu = 0) %>%
  rowwise() %>%
  mutate(score = cubrimiento_empirico_mu(mu = mu, 
                                         sigma = sigma, 
                                         nivel = nivel, 
                                         n = n, 
                                         Nrep = Nrep, 
                                         seed = 1)) %>%
  dplyr::select(all_of(c("sigma", "n", "score"))) %>%
  pivot_wider(names_from = n, values_from = score)
cubrimientos

# El cubrimiento depende de n y no de sigma.

# Ejercicio 4 ####
# Es lo mismo que en el 1 b pero hecho función
hallar_n_mu <- function(sigma, nivel, L) {
  alfa <- 1-nivel
  ceiling((2*qnorm(1-alfa/2)/L)^2 * sigma^2)
}

hallar_n_mu(sigma = 1/2, nivel = 0.95, L = 0.1)

# Ejercicio 5 ####
Nrep <- 1000
n <- 5
mu <- 3
sigma2 <- 4

valores_t <- rep(NA, Nrep)
for (i in 1:Nrep) {
  x = rnorm(n = n, mean = mu, sd = sqrt(sigma2))
  valores_t[i] = (mean(x)-mu)/(sd(x)/sqrt(n))
}

densities <- tibble(x = density(valores_t)$x,
                    dens_np = density(valores_t)$y) %>%
  rowwise() %>%
  mutate(t = dt(x = x, df = n-1)) 

densities %>%
  ggplot(aes(x = x)) +
  geom_histogram(data = tibble(valores_t),
                 aes(x = valores_t, y = after_stat(density)),
                 alpha = .5) + 
  geom_line(aes(y = dens_np), color = "black") +
  geom_line(aes(y = t), color = "red") +
  theme_bw()

# Ejercicio 6 ####
intervalo_mu_exacto <- function(nivel, desvio, x, verbose = F) {
  n <- length(x)
  alfa <- 1-nivel
  if (verbose) {
    cat(paste0("El alfa es: ", alfa, " - Y el nivel es: ", nivel, "\n")) 
  }
  if (is.null(desvio)) {
    ci_low <- mean(x)+qt(df = n-1, p = alfa/2)*sd(x)*sqrt(1/n)
    ci_up <- mean(x)+qt(df = n-1, p = 1-alfa/2)*sd(x)*sqrt(1/n)
  } else {
    ci_low <- mean(x)+qnorm(p = alfa/2)*desvio*sqrt(1/n)
    ci_up <- mean(x)+qnorm(p = 1-alfa/2)*desvio*sqrt(1/n)
  }
  c(ci_low, ci_up)
}

intervalo_mu_exacto(nivel = 1-beta, 
                    desvio = sqrt(var), 
                    x = x,
                    verbose = T)

intervalo_mu_exacto(nivel = 1-beta, 
                    desvio = NULL, 
                    x = x,
                    verbose = T)

# Ejercicio 7 ####
## a #### 
cubrimiento_empirico_mu <- function(mu, sigma, nivel, n, Nrep, seed, verbose = F) {
  set.seed(seed)
  cubrimiento <- rep(NA, Nrep)
  longitud <- rep(NA, Nrep)
  if (verbose) {
    cat(paste0("El alfa es: ", 1-nivel, " - Y el nivel es: ", nivel, "\n"))   
  }
  for (i in 1:Nrep) {
    x <- rnorm(n = n, mean = mu, sd = sigma)
    ci <- intervalo_mu_exacto(nivel = nivel, 
                              desvio = NULL, 
                              x = x)
    cubrimiento[i] <- (mu>ci[1]) & (mu<ci[2])
    longitud[i] <- ci[2]-ci[1]
  }
  tibble(cubrimiento = mean(cubrimiento),
         longitud = mean(longitud))
}

cubrimiento_empirico_mu(mu = 5, 
                        sigma = 0.2, 
                        nivel = 0.95, 
                        n = 100, 
                        Nrep = 10000, 
                        seed = 5,
                        verbose = T)

## b ####
cubrimientos <- expand_grid(n = c(5, 10, 30, 50),
                            sigma = c(0.1, 1, 10),
                            Nrep = 1000, 
                            nivel = 0.95,
                            mu = 0) %>%
  rowwise() %>%
  mutate(score = cubrimiento_empirico_mu(mu = mu, 
                                         sigma = sigma, 
                                         nivel = nivel, 
                                         n = n, 
                                         Nrep = Nrep, 
                                         seed = 1)$cubrimiento,
         longitud = cubrimiento_empirico_mu(mu = mu, 
                                            sigma = sigma, 
                                            nivel = nivel, 
                                            n = n, 
                                            Nrep = Nrep, 
                                            seed = 1)$longitud) 
cubrimientos

# Si bien el cubrimiento depende , la longitud es proporcional a sigma.

# Ejercicio 8 ####
Nrep <- 1000
n <- 5
mu <- 3
sigma2 <- 4

valores_chi <- rep(NA, Nrep)
for (i in 1:Nrep) {
  x = rnorm(n = n, mean = mu, sd = sqrt(sigma2))
  valores_chi[i] = sum((x-mean(x))^2)/sigma2
}

densities <- tibble(x = density(valores_chi)$x,
                    dens_np = density(valores_chi)$y) %>%
  rowwise() %>%
  mutate(chi = dchisq(x = x, df = n-1)) 

densities %>%
  ggplot(aes(x = x)) +
  geom_histogram(data = tibble(valores_chi),
                 aes(x = valores_chi, y = after_stat(density)),
                 alpha = .5) + 
  geom_line(aes(y = dens_np), color = "black") +
  geom_line(aes(y = chi), color = "red") +
  theme_bw()

# Ejercicio 9 ####
x <- c(1.52, 1.65, 1.72, 1.65, 1.72, 1.83, 1.62, 1.75, 1.72, 1.68, 1.51, 1.65, 1.58,
       1.65, 1.61, 1.70, 1.60, 1.73, 1.61, 1.52, 1.81, 1.72, 1.50, 1.82, 1.65)

## a ####
intervalo_mu_exacto(nivel = 0.95, 
                    desvio = NULL, 
                    x = x, 
                    verbose = T)

## b ####
intervalo_sd2_exacto <- function(nivel, x, verbose = F) {
  alfa <- 1-nivel
  n <- length(x)
  if (verbose) {
    cat(paste0("El alfa es: ", 1-nivel, " - Y el nivel es: ", nivel, "\n"))   
  }
  ci_low <- (n-1)*sd(x)^2/(qchisq(p = 1-alfa/2, df = n-1))
  ci_up <- (n-1)*sd(x)^2/(qchisq(p = alfa/2, df = n-1))
  c(ci_low, ci_up)
}

intervalo_sd2_exacto(nivel = 0.9, 
                     x = x,
                     verbose = T)

cubrimiento_empirico_sd2 <- function(sigma, nivel, n, Nrep, seed, verbose = F) {
  set.seed(seed)
  cubrimiento <- rep(NA, Nrep)
  if (verbose) {
    cat(paste0("El alfa es: ", 1-nivel, " - Y el nivel es: ", nivel, "\n"))   
  }
  for (i in 1:Nrep) {
    x <- rnorm(n = n, mean = 0, sd = sigma)
    ci <- intervalo_sd2_exacto(nivel = nivel, 
                               x = x)
    cubrimiento[i] <- (sigma>sqrt(ci[1])) & (sigma<sqrt(ci[2]))
  }
  mean(cubrimiento)
}

cubrimiento_empirico_sd2(sigma = 0.2, 
                         nivel = 0.9, 
                         n = 100, 
                         Nrep = 10000, 
                         seed = 1,
                         verbose = T)

# Ejercicio 10 ####
intervalo_lambda_exacto <- function(alpha, x) {
  n <- length(x)
  ci_low <- qchisq(df = 2*n, p = alpha/2) / (2*mean(x)*n)
  ci_up <- qchisq(df = 2*n, p = 1-alpha/2) / (2*mean(x)*n)
  c(ci_low, ci_up)
}

x <- c(25, 45, 50, 61, 39, 40, 45, 47, 38, 39, 54, 60, 39, 46, 39, 50, 42, 50, 62, 50)

intervalo_lambda_exacto(alpha = .1, x = x)
rev(1/intervalo_lambda_exacto(alpha = .1, x = x))
