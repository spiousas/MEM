pacman::p_load(here, tidyverse)

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

## Entrega ####
data <- read_csv(here("metodos_estimacion/TPs/entrega_1/entrega1_1-data.csv")) %>%
  pivot_longer(cols = !NOMBRE, names_to = "n", values_to = "p") %>%
  mutate(n = as.numeric(n))
data

data %>% ggplot(aes(x = factor(n),
                    y = p,
                    color = factor(n))) +
  geom_boxplot() +
  labs(title = "Boxplot paralelos del estimador p (22 alumnos)",
       x = "N",
       y = "Estimación de p") +
  theme_bw() +
  theme(legend.position = "none")

data %>% mutate(N=factor(n)) %>%
  ggplot(aes(x = p,
             fill = N)) +
  geom_histogram() +
  labs(title = "Histogramas del estimador p (22 alumnos)",
       x = "Estimación de p") +
  facet_grid(N~., labeller = label_both) +
  theme_bw() +
  theme(legend.position = "none")

# Ejercicio 5 ####
mu <- 10
sd <- 2
datos <- mu + rnorm(mean = 0, sd = sd, n = 60) 

# a ####
# Estimo mu
mu <- mean(datos)
mu

# b #### 
# Estimo sigma^2
# Esto es partir de V(x) = E(X^2) - E(X)^2
sigma2 <- 1/length(datos) * sum(datos^2) - (mean(datos))^2
sigma2

# Otra forma haciendo V(x) = mean((X-mean(X)^2) (el que usamos para bootstraping)
mean((datos - mean(datos))^2)

# c ####
# Estimo sigma^2 con el estimador insesgado
S <- sd(datos)
S^2
var(datos)

# d ####
# Estimo el error estándar con un sigma conocido
sigma0 <- 2
sqrt(sigma0/length(datos))

# e ####
# Sesgo del estimador sesgado de sigma^2 porque
# E(sigma^2) = (n-1)/n * sigma0^2
-sigma0/length(datos)

# f ####
# Sesgo del estimador insesgado de sigma^2
0

# g ####
sqrt(S^2/length(datos))

# Ejercicio 6 ####
theta <- 5
datos <- runif(n = 60, min = 0, max = theta)

# a ####
# Estimador de momentos de la uniforme
2*mean(datos)

# b ####
# Estimador de MV de la uniforme
max(datos)

# Ejercicio 7 ####
## Estimadores ####
est1_unif <- function(x) {
  2*mean(x)
}

est2_unif <- function(x) {
  max(x)
}

## Histogramas ####
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

## Densidades ####
densidad_est1_unif <- function(theta, n, Nrep, seed) {
  set.seed(seed)
  est_1 <- c()
  for (i in 1:Nrep) {
    est_1[i] <- est1_unif(runif(n = n, min = 0, max = theta))
  }
  plot(density(est_1))
}

densidad_est1_unif(3, 100, 1000, 1)

densidad_est2_unif <- function(theta, n, Nrep, seed) {
  set.seed(seed)
  est_2 <- c()
  for (i in 1:Nrep) {
    est_2[i] <- est2_unif(runif(n = n, min = 0, max = theta))
  }
  plot(density(est_2))
}

densidad_est2_unif(3, 20, 100, 1)

## d ####
ECME_est1_unif <- function(theta, n, Nrep, seed) {
  set.seed(seed)
  est_1 <- c()
  for (i in 1:Nrep) {
    est_1[i] <- est1_unif(runif(n = n, min = 0, max = theta))
  }
  1/Nrep * sum((est_1-theta)^2)
}

ECME_est2_unif <- function(theta, n, Nrep, seed) {
  set.seed(seed)
  est_2 <- c()
  for (i in 1:Nrep) {
    est_2[i] <- est2_unif(runif(n = n, min = 0, max = theta))
  }
  1/Nrep * sum((est_2-theta)^2)
}

## e ####
# ECM teórico para el de Momentos -> ECM_{\theta}(\hat{\theta}) = \frac{\theta^2}{3n}
ECM_est1_unif <- function(theta, n) {
  theta^2/(3*n)
}

# ECM teórico para el de MV -> ECM_{\theta}(\tilde{\theta}) = \frac{2\theta^2}{(n+1)(n+2)}
ECM_est2_unif <- function(theta, n) {
  2*theta^2/((n+1)*(n+2))
}