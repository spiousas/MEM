pacman::p_load(tidyverse, here)

# Ejercicio 1 ####
## a ####
n <- 10
beta0 <- 5
beta1 <- 1
beta2 <- 3 

X1 <- runif(n,0,10)
X2 <- runif(n,0,10)

e <- rnorm(n)
Y <- beta0 + beta1*X1 + beta2*X2 + e
Y

data <- tibble(X1, X2, Y)
data
model <- lm(data = data, Y ~ X1 + X2)
summary(model)

## b ####
Nrep <- 1000
betas1 <- rep(NA, Nrep)
betas2 <- rep(NA, Nrep)
sd_beta1 <- rep(NA, Nrep)
sd_beta2 <- rep(NA, Nrep)

set.seed(1234)
for (i in 1:Nrep) {
  e <- rnorm(n)
  Y <- beta0 + beta1*X1 + beta2*X2 + e
  
  data <- tibble(X1, X2, Y)
  model <- lm(data = data, Y ~ X1 + X2)
  
  betas1[i] <- model$coefficients[2]
  betas2[i] <- model$coefficients[3]
  sd_beta1[i] <- summary(model)$coefficients[2,2]
  sd_beta2[i] <- summary(model)$coefficients[3,2]
  
}

## c ####
hist(betas1)

## d ####
# Los betas estimados vienen de una normal multivariada de medias betas y 
# matriz de covarianza dada por sigma^2*(t(X) %*% X)^(-1), donde sigma^2 es la 
# varianza del error (1 en nuestro caso).

model <- lm(data = data, Y ~ X1 + X2)
X <- model.matrix(model)
mu <- matrix(c(beta1, beta2), nrow = 2)
mu
var <- inv(t(X) %*% X)
var[2,2]
var[3,3]

## e ####
T <- (betas1 - beta1)/sd_beta1

## f, g y h ####
tibble(T) %>% 
  ggplot() +
  geom_histogram(aes(x = T, y = after_stat(..density..)), alpha = .5) + 
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = sqrt(1)), color = "red") +
  stat_function(fun = dt, n = 101, args = list(df = n-2), color = "blue") +
  theme_bw()

## i y j ####
cubrimiento_beta_1 <- rep(NA, Nrep)
cubrimiento_beta_2 <- rep(NA, Nrep)

set.seed(1234)
for (i in 1:Nrep) {
  e <- rnorm(n)
  Y <- beta0 + beta1*X1 + beta2*X2 + e
  
  data <- tibble(X1, X2, Y)
  model <- lm(data = data, Y ~ X1 + X2)
  
  CI <- confint(model, level = .9)
  
  cubrimiento_beta_1[i] <- between(beta1, CI[2,1], CI[2,2])
  cubrimiento_beta_2[i] <- between(beta2, CI[3,1], CI[3,2])
}

sum(cubrimiento_beta_1)/Nrep
sum(cubrimiento_beta_2)/Nrep

## k ####
sum(cubrimiento_beta_1 & cubrimiento_beta_2)/Nrep

# Ejercicio 2 ####
n <- 150
X1 <- runif(n,0,10)
X2 <- runif(n,0,10)

## La simulación ####
Nrep <- 1000
betas1 <- rep(NA, Nrep)
betas2 <- rep(NA, Nrep)
sd_beta1 <- rep(NA, Nrep)
sd_beta2 <- rep(NA, Nrep)
cubrimiento_beta_1 <- rep(NA, Nrep)
cubrimiento_beta_2 <- rep(NA, Nrep)

set.seed(1234)
for (i in 1:Nrep) {
  e <- rnorm(n)
  Y <- beta0 + beta1*X1 + beta2*X2 + e
  
  data <- tibble(X1, X2, Y)
  model <- lm(data = data, Y ~ X1 + X2)
  
  betas1[i] <- model$coefficients[2]
  betas2[i] <- model$coefficients[3]
  sd_beta1[i] <- summary(model)$coefficients[2,2]
  sd_beta2[i] <- summary(model)$coefficients[3,2]
  
  CI <- confint(model, level = .9)
  
  cubrimiento_beta_1[i] <- between(beta1, CI[2,1], CI[2,2])
  cubrimiento_beta_2[i] <- between(beta2, CI[3,1], CI[3,2])
}

hist(betas1)

## Estadístico T ####
T <- (betas1 - beta1)/sd_beta1

tibble(T) %>% 
  ggplot() +
  geom_histogram(aes(x = T, y = after_stat(..density..)), alpha = .5) + 
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = sqrt(1)), color = "red") +
  stat_function(fun = dt, n = 101, args = list(df = n-2), color = "blue") +
  theme_bw()

## CIs y cubrimiento ####
model <- lm(data = data, Y ~ X1 + X2)
confint(model, level = .9)
# Los intervalos de confianza son mucho más finos, es decir, la estimación de 
# beta_1 y beta_2 es más precisa.

sum(cubrimiento_beta_1)/Nrep
sum(cubrimiento_beta_2)/Nrep

sum(cubrimiento_beta_1 & cubrimiento_beta_2)/Nrep

# Ejercicio 3 ####
## Con n=10 ####
n <- 10
X1 <- runif(n,0,10)
X2 <- runif(n,0,10)

### La simulación ####
Nrep <- 1000
betas1 <- rep(NA, Nrep)
betas2 <- rep(NA, Nrep)
sd_beta1 <- rep(NA, Nrep)
sd_beta2 <- rep(NA, Nrep)
cubrimiento_beta_1 <- rep(NA, Nrep)
cubrimiento_beta_2 <- rep(NA, Nrep)

set.seed(1234)
for (i in 1:Nrep) {
  e <- rnorm(n)
  Y <- beta0 + beta1*X1 + beta2*X2 + e
  
  data <- tibble(X1, X2, Y)
  model <- lm(data = data, Y ~ X1 + X2)
  
  betas1[i] <- model$coefficients[2]
  betas2[i] <- model$coefficients[3]
  sd_beta1[i] <- summary(model)$coefficients[2,2]
  sd_beta2[i] <- summary(model)$coefficients[3,2]
  
  CI <- confint(model, level = .9)
  
  cubrimiento_beta_1[i] <- between(beta1, CI[2,1], CI[2,2])
  cubrimiento_beta_2[i] <- between(beta2, CI[3,1], CI[3,2])
}

hist(betas1)

### Estadístico T ####
T <- (betas1 - beta1)/sd_beta1

tibble(T) %>% 
  ggplot() +
  geom_histogram(aes(x = T, y = after_stat(..density..)), alpha = .5) + 
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = sqrt(1)), color = "red") +
  stat_function(fun = dt, n = 101, args = list(df = n-2), color = "blue") +
  theme_bw()

### CIs y cubrimiento ####
sum(cubrimiento_beta_1)/Nrep
sum(cubrimiento_beta_2)/Nrep

sum(cubrimiento_beta_1 & cubrimiento_beta_2)/Nrep

## Con n=150 ####
n <- 150
X1 <- runif(n,0,10)
X2 <- runif(n,0,10)

### La simulación ####
Nrep <- 1000
betas1 <- rep(NA, Nrep)
betas2 <- rep(NA, Nrep)
sd_beta1 <- rep(NA, Nrep)
sd_beta2 <- rep(NA, Nrep)
cubrimiento_beta_1 <- rep(NA, Nrep)
cubrimiento_beta_2 <- rep(NA, Nrep)

set.seed(1234)
for (i in 1:Nrep) {
  e <- rnorm(n)
  Y <- beta0 + beta1*X1 + beta2*X2 + e
  
  data <- tibble(X1, X2, Y)
  model <- lm(data = data, Y ~ X1 + X2)
  
  betas1[i] <- model$coefficients[2]
  betas2[i] <- model$coefficients[3]
  sd_beta1[i] <- summary(model)$coefficients[2,2]
  sd_beta2[i] <- summary(model)$coefficients[3,2]
  
  CI <- confint(model, level = .9)
  
  cubrimiento_beta_1[i] <- between(beta1, CI[2,1], CI[2,2])
  cubrimiento_beta_2[i] <- between(beta2, CI[3,1], CI[3,2])
}

hist(betas1)

### Estadístico T ####
T <- (betas1 - beta1)/sd_beta1

tibble(T) %>% 
  ggplot() +
  geom_histogram(aes(x = T, y = after_stat(..density..)), alpha = .5) + 
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = sqrt(1)), color = "red") +
  stat_function(fun = dt, n = 101, args = list(df = n-2), color = "blue") +
  theme_bw()

### CIs y cubrimiento ####
sum(cubrimiento_beta_1)/Nrep
sum(cubrimiento_beta_2)/Nrep

sum(cubrimiento_beta_1 & cubrimiento_beta_2)/Nrep

## Conclusiones ####
# En ambos casos el cubrimiento es muy bueno, aunque con n=10 la distribución de beta_1
# pareciera estar sesgada.
