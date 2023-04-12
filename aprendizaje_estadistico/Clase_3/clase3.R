library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

# Practica 3 ####
# Ej 1 ####
## 1 ####
a <- c(1.17, 1.75, 0.28, 2.56, 2.36, 0.36, 1.82, 0.24, 1.17, 1.86)
b <- c(0.66, 0.07, 0.62, 0.65, 1.33, 0.40, 1.17, 1.11, 2.01, 2.98)

est1 <- function(x) {
  2*mean(x)
}

est2 <- function(x) {
  max(x)
}

est1(a)
est2(a)
est1(b)
est2(b)

## 2 ####
set.seed(123)
x <- runif(n = 5, min = 0, max = 3)
x

## 3 ####
est1(x)

## 4, 5 y 6 ####
m <- 1000
set.seed(123)
est1_vec_5 <- 1:m
est2_vec_5 <- 1:m
est1_vec_30 <- 1:m
est2_vec_30 <- 1:m
est1_vec_50 <- 1:m
est2_vec_50 <- 1:m
est1_vec_100 <- 1:m
est2_vec_100 <- 1:m

set.seed(123)
for (i in seq(1,m)) {
  x_5 <- runif(n = 5, min = 0, max = 3)
  est1_vec_5[i] <- est1(x_5)
  est2_vec_5[i] <- est2(x_5)
}
set.seed(123)
for (i in seq(1,m)) {
  x_30 <- runif(n = 30, min = 0, max = 3)
  est1_vec_30[i] <- est1(x_30)
  est2_vec_30[i] <- est2(x_30)
}
set.seed(123)
for (i in seq(1,m)) {
  x_50 <- runif(n = 50, min = 0, max = 3)
  est1_vec_50[i] <- est1(x_50)
  est2_vec_50[i] <- est2(x_50)
}
set.seed(123)
for (i in seq(1,m)) {
  x_100 <- runif(n = 100, min = 0, max = 3)
  est1_vec_100[i] <- est1(x_100)
  est2_vec_100[i] <- est2(x_100)
}

par(mfrow = c(4,2))

hist(est1_vec_5, xlim=c(0, 6), prob = TRUE)
hist(est2_vec_5, xlim=c(0, 6), prob = TRUE)
hist(est1_vec_30, xlim=c(0, 6), prob = TRUE)
hist(est2_vec_30, xlim=c(0, 6), prob = TRUE)
hist(est1_vec_50, xlim=c(0, 6), prob = TRUE)
hist(est2_vec_50, xlim=c(0, 6), prob = TRUE)
hist(est1_vec_100, xlim=c(0, 6), prob = TRUE)
hist(est2_vec_100, xlim=c(0, 6), prob = TRUE)

m_vec <- c(rep(5, 2*m), rep(30, 2*m), rep(50, 2*m), rep(100, 2*m))
est_vec <- c(rep(1, m), rep(2, m), rep(1, m), rep(2, m), rep(1, m), rep(2, m), rep(1, m), rep(2, m))
data <- tibble(m = m_vec,
               est = est_vec,
               value = c(est1_vec_5, est2_vec_5, est1_vec_30, est2_vec_30, est1_vec_30, est2_vec_50, est1_vec_100, est2_vec_100))

data %>% ggplot(aes(x = factor(m), y = value, color = factor(est), fill = factor(est))) +
  geom_hline(yintercept = 3, alpha  = .3) +
  geom_boxplot(alpha = .2) +
  labs(x = "Tamaño de la muestra",
       y = "Valor del estimador",
       color = "Tipo de estimador", 
       fill = "Tipo de estimador") +
  theme_bw() +
  theme(legend.position = "top")

# Ej 3 ####
## 1 ####
data_summ <- data %>% group_by(m, est) %>%
  summarise(bias = mean(value)-3,
            var = mean((value-mean(value))^2),
            EMSE = mean((value-3)^2))
data_summ
            