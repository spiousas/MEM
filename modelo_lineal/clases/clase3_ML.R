library(dplyr, readr, ggplot2)
library(ggplot2)
# Generamos la muestra de x
set.seed(1234)
## a #####
x <- runif(n = 60, min = 0, max = 20)
x

## b #####
sigma <- 1
e <-rnorm(n = 60, mean = 0, sd = sigma)
y <- 4-2*x+e

## c #####
data <- tibble(x, y)

model0 <- lm(data = data, y ~ x)
model0

data %>% ggplot(aes(x=x, y=y)) +
  geom_point() +
  geom_abline(intercept = model0$coefficients[1], 
              slope = model0$coefficients[2], 
              color = "blue") +
  theme_bw()

## d #####
Nrep <- 1000
beta0 <- c()
beta1 <- c()

for (i in 1:Nrep) {
  e <- rnorm(n = 60, mean = 0, sd = sigma)
  y <- 4-2*x+e
  model <- lm(y ~ x)
  beta0[i] <- model$coefficients[1]
  beta1[i] <- model$coefficients[2]
}

cov(beta0, beta1)

## f #####
pairs(cbind(beta0, beta1))
cor(beta0, beta1)

coefs <- tibble(beta0, beta1)

coefs %>% ggplot(aes(x = beta0,
                     y = beta1)) +
  geom_point() +
  theme_bw()

## h #####
coefs %>% ggplot(aes(x = beta0)) +
  geom_histogram() +
  theme_bw()

coefs %>% ggplot(aes(x = beta1)) +
  geom_histogram() +
  theme_bw()

## i #####
Nrep <- 1000
beta0 <- c()
beta1 <- c()
beta0_norm_conocido <- c()
beta1_norm_conocido <- c()
beta0_norm_desconocido <- c()
beta1_norm_desconocido <- c()
n <- 10
sigma <- 1

x <- runif(n = n, min = 0, max = 20)

for (i in 1:Nrep) {
  e <- rnorm(n = n, mean = 0, sd = sigma)
  y <- 4-2*x+e
  model <- lm(y ~ x)
  beta0[i] <- model$coefficients[1]
  beta1[i] <- model$coefficients[2]
  beta0_norm_conocido[i] <- (beta0[i]-4)/(sigma*sqrt(1/n+mean(x)^2/((n-1)*sd(x)^2)))
  beta1_norm_conocido[i] <- (beta1[i]-(-2))/(sigma/sqrt((n-1)*sd(x)^2))
  beta0_norm_desconocido[i] <- (beta0[i]-4)/ (summary(model)$sigma / sd(x))
  beta1_norm_desconocido[i] <- (beta1[i]-(-2))/ (summary(model)$sigma / sd(x))
}


coefs <- tibble(beta0, beta1, beta0_norm_conocido, beta1_norm_conocido,
                beta0_norm_desconocido, beta0_norm_desconocido)

coefs %>% ggplot(aes(x = beta0_norm_conocido)) +
  geom_histogram(aes(y = after_stat(..density..))) +
  stat_function(fun = dnorm, n = 100, args = list(mean = 0, sd = 1), color = "red") +
  theme_bw()

coefs %>% ggplot(aes(x = beta1_norm_conocido)) +
  geom_histogram(aes(y = after_stat(..density..))) +
  stat_function(fun = dnorm, n = 100, args = list(mean = 0, sd = 1), color = "red") +
  theme_bw()

coefs %>% ggplot(aes(x = beta0_norm_desconocido)) +
  geom_histogram(aes(y = after_stat(..density..))) +
  stat_function(fun = dt, n = 100, args = list(df = n-2), color = "red") +
  theme_bw()

coefs %>% ggplot(aes(x = beta1_norm_desconocido)) +
  geom_histogram(aes(y = after_stat(..density..))) +
  stat_function(fun = dt, n = 100, args = list(df = n-2), color = "red") +
  theme_bw()
