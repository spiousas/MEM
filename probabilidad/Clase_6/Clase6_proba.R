library(tibble)
library(dplyr)
library(ggplot2)
library(tidyr)

# 2.1 ####
## 1 ####
promedio <- function(n_ifty, dist = "lognormal", theta = c(0,1)) {
  prom <-  rep(NA, n_ifty)

  if (dist == "normal") {
    sample <- rnorm(n = n_ifty, mean = theta[1], sd = theta[2])
    for (i in 1:n_ifty) {
      prom[i] <- mean(sample[1:i])
    }  
  }
  
  if (dist == "lognormal") {
    sample <- exp(rnorm(n = n_ifty, mean = theta[1], sd = theta[2]))
    for (i in 1:n_ifty) {
      prom[i] <- mean(sample[1:i])
    }  
  }
  
  if (dist == "unif") {
    sample <- runif(n = n_ifty, min = theta[1], max = theta[2])
    for (i in 1:n_ifty) {
      prom[i] <- mean(sample[1:i])
    }  
  }
  
  prom
}

## 2 ####
prom_lognormal <- promedio(30000, dist = "lognormal")
plot(prom_lognormal)
prom_lognormal[30000]
exp(1/2)
# La esperanza de la lognormal es exp(mu+sigma^2/2) y corresponde con el lugar
# al que converge

## 3 ####
muchos_promedios <- function(N_gente, n_ifty, dist = "lognormal", theta = c(0,1)) {
  proms <- matrix(nrow = N_gente, ncol = n_ifty)
  for (i in 1:N_gente) {
    proms[i,] <- promedio(n_ifty = n_ifty, dist = dist, theta = theta) 
  }
  
  proms
}

## 4 ####
N_gente <- 400
n_ifty <- 10000
dist <- "lognormal"
theta <- c(0, 1)
promedios <- muchos_promedios(N_gente = N_gente, 
                              n_ifty = n_ifty,
                              dist = dist,
                              theta = theta)

promedios_tbl <- as_tibble(t(promedios)) %>% 
  mutate(n = row_number()) %>%
  pivot_longer(cols = !n)

promedios_tbl %>% ggplot(aes(x = n,
                             y = value,
                             color = name)) +
  geom_line(alpha = .5) +
  geom_hline(yintercept = exp(1/2)) +
  theme_bw() +
  theme(legend.position = "none")

promedios_tbl %>% filter(n == n_ifty) %>%
  ggplot(aes(x = value)) +
  geom_histogram(alpha = .5) +
  geom_vline(xintercept = exp(1/2)) +
  theme_bw() +
  theme(legend.position = "none")

## 5 ####
params <- function(dist, theta) {
  if (dist == "lognormal") {
    esp <- exp(theta[1]+theta[2]^2/2)
    var <- (exp(theta[2]^2) - 1) * exp(2*theta[1]+theta[2]^2)  
  }
  
  c(esp, var)
}
# Prueba
params(dist = dist, theta = theta)

scale <- function(x, dist, theta){
  if (dist == "lognormal") {
    pars <- params(dist = dist, theta = theta)
    y <- (x - pars[1]) / pars[2]
  }
  
  y
} 
# Prueba
scale(x = promedios[1,], dist = dist, theta = theta)[n_ifty]

promedios_tbl <- promedios_tbl %>% 
  mutate(norm_value = scale(x = value, dist = dist, theta = theta))

promedios_tbl %>% filter(n == n_ifty) %>%
  ggplot() +
  geom_histogram(aes(norm_value, y = ..density..), 
                 alpha = .5) +
  #geom_function(aes(colour = "normal"), fun = dnorm) +
  # stat_function(fun = dnorm,
  #               args = list(mean = 0,
  #                           sd = 1,
  #               col = "#1b98e0",
  #               size = 2)) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(legend.position = "none")

