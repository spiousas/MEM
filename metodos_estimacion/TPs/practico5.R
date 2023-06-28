library(tidyverse)
library(knitr)
library(kableExtra)

# Ejercicio 1 ####
mean_x <- 15.22
mean_y <- 15.30
n1 <- 12
n2 <- 11
sigma <- 0.1
level <- 0.95
alpha <- 1 - level

mean_x - mean_y - qnorm(1-alpha/2) * sigma * sqrt(1/n1 + 1/n2)
mean_x - mean_y + qnorm(1-alpha/2) * sigma * sqrt(1/n1 + 1/n2)

# Ejercicio 2 ####
x <- c(1.43, 1.27, 1.48, 1.53, 1.71, 1.72)
y <- c(1.42, 1.24, 1.39, 1.41, 1.6, 1.61)
z <- x-y
n <- length(z)
level <- 0.95
alpha <- 1 - level

mean(z) - qt(df = n-1, p = 1-alpha/2) * sqrt(var(z)/n)
mean(z) + qt(df = n-1, p = 1-alpha/2) * sqrt(var(z)/n)

t.test(x, y, paired = TRUE)

# Ejercicio 3 ####
x <- c(94, 90, 100, 104, 100, 100, 96, 98, 100, 105, 99, 108, 101, 99)
n1 <- length(x)
m1 <- mean(x)
s12 <- var(x)
y <- c(105, 109, 93, 113, 113, 106, 108, 117, 113, 102, 95, 108, 117, 105)
n2 <- length(y)
m2 <- mean(y)
s22 <- var(y)

sp2 <- (s12 * (n1-1) + s22 * (n2-1)) / (n1+n2-2)

level <- 0.95
alpha <- 1 - level

m1 - m2 - qt(df = n1+n2-2, p = 1-alpha/2) * sqrt(sp2 * (1/n1 + 1/n2))
m1 - m2 + qt(df = n1+n2-2, p = 1-alpha/2) * sqrt(sp2 * (1/n1 + 1/n2))

t.test(x, y, level = level, var.equal = TRUE)

# Ejercicio 4 ####
n <- 100
curados <- 30
phat <- curados/n

nivel <- 0.95
alfa <- 1-nivel

## b-i ####
phat - qnorm(1-alfa/2) * sqrt(phat*(1-phat)/n)
phat + qnorm(1-alfa/2) * sqrt(phat*(1-phat)/n)

## b-ii ####
lmax <- 0.1

ceiling((qnorm(1-alfa/2) / lmax)^2)

# Ejercicio 5 ####
x <- c(35, 41, 38, 40, 34, 36, 41, 48, 42, 46, 39, 37, 41, 35, 37, 38, 42, 43, 44, 67) 
n <- length(x)

nivel <- 0.9
alfa <- 1 - nivel

# A partir de EMV
CIpoisEMV <- function(x, nivel) {
  n <- length(x)  
  alfa <- 1 - nivel
  c(mean(x), mean(x) - qnorm(1-alfa/2) * sqrt(mean(x)/n) ,mean(x) + qnorm(1-alfa/2) * sqrt(mean(x)/n))
}
CIpoisEMV(x, nivel)

# Despejando a lo bestia
CIpois <- function(x, nivel) {
  n <- length(x)  
  alfa <- 1 - nivel
  Z <- qnorm(1-alfa/2)
  c(mean(x) + Z^2/(2*n), mean(x) + Z^2/(2*n) - Z/2 * sqrt(4 * mean(x)/n + Z^2/n^2), mean(x) + Z^2/(2*n) + Z/2 * sqrt(4 * mean(x)/n + Z^2/n^2))
}
CIpois(x, nivel)

# Comparemos ambos
comparaCI <- function(n, nivel) {
  x <- rpois(lambda = 40, n = n)
  c(CIpoisEMV(x, nivel), CIpois(x, nivel)) 
}
comparaCI(2, nivel)

data <- expand_grid(method = c("EMV", "full"), n = 5:150) %>%
  rowwise() %>%
  mutate(center = if_else(method == "EMV", comparaCI(n, nivel)[1], comparaCI(n, nivel)[4]),
         min = if_else(method == "EMV", comparaCI(n, nivel)[2], comparaCI(n, nivel)[5]),
         max = if_else(method == "EMV", comparaCI(n, nivel)[3], comparaCI(n, nivel)[6]))

data %>% ggplot(aes(x = n,
           y = center,
           color = method,
           fill = method)) +
  geom_hline(yintercept = 40) +
  geom_line() +
  geom_ribbon(aes(ymin = min, ymax = max), alpha = .3) +
  theme_bw()

# Ejercicio 7 ####
estimar_p_intervalo <- function(nivel, x, metodo = 1) {
  alfa <- 1 - nivel
  Z <- qnorm(1-alfa/2)
  n <- length(x)
  if (metodo == 1) {
  int <- c(mean(x) - Z * sqrt(mean(x)*(1-mean(x))/n), 
           mean(x) + Z * sqrt(mean(x)*(1-mean(x))/n))  
  } else {
    a <- n^2 + n*Z^2
    b <- -2*n*sum(x) - Z^2*n
    c <- (sum(x))^2
    int <- c((-b - sqrt(b^2-4*a*c)) / (2*a), 
             (-b + sqrt(b^2-4*a*c)) / (2*a)) 
  }
  int
}
x <- rbinom(prob = .5, n = 100000, size = 1)
estimar_p_intervalo(.95, x, metodo = 2)

hallar_n_p <- function(nivel, lmax) {
  alfa <- 1 - nivel
  ceiling((qnorm(1-alfa/2) / lmax)^2) 
}
hallar_n_p(.95, 0.1)

# Ejercicio 8 ####
calcular_cubrimiento_empirico <- function(p, nivel, n, Nrep, semilla, metodo = 1) {
  cubrimiento <- rep(NA, Nrep)
  long <- rep(NA, Nrep)
  set.seed(semilla)
  for (i in 1:Nrep) {
    x <- rbinom(prob = p, n = n, size = 1)
    int <- estimar_p_intervalo(nivel, x, metodo)
    cubrimiento[i] <- (p>=int[1]) & (p<=int[2])
    long[i] <- diff(int)
  } 
  paste0(round(mean(cubrimiento), digits = 3)," (", round(mean(long), digits = 3), ")")
}

calcular_cubrimiento_empirico(p = .5, nivel = .95, n = 100, Nrep = 1000, semilla = 2)

data_1 <- expand_grid(n = c(5, 10, 30, 50, 100, 1000),
                    p = c(0.1, 0.5)) %>%
  rowwise() %>%
  mutate(result = calcular_cubrimiento_empirico(p = p, nivel = .95, n = n, Nrep = 1000, semilla = 2, metodo = 1)) %>%
  pivot_wider(names_from = n, values_from = result) 

data_1 %>%
  kbl(caption = "Metodo 1", # Adding caption  
      format = "latex") %>% # Output format = latex 
  kable_classic(html_font = "Cambria") # Font = Cambria 

data_2 <- expand_grid(n = c(5, 10, 30, 50, 100, 1000),
                      p = c(0.1, 0.5)) %>%
  rowwise() %>%
  mutate(result = calcular_cubrimiento_empirico(p = p, nivel = .95, n = n, Nrep = 1000, semilla = 2, metodo = 2)) %>%
  pivot_wider(names_from = n, values_from = result) 

data_2 %>%
  kbl(caption = "Metodo 2", # Adding caption  
      format = "latex") %>% # Output format = latex 
  kable_classic(html_font = "Cambria") # Font = Cambria 

