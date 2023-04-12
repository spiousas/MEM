# Ejercicio 2 ####
# Genero los datos
library(tidyverse)
est1 <- function(x) {
  2*mean(x)
}

est2 <- function(x) {
  max(x)
}

theta <- 3
m <- 1000
est1_vec_5 <- 1:m
est2_vec_5 <- 1:m
est1_vec_30 <- 1:m
est2_vec_30 <- 1:m
est1_vec_50 <- 1:m
est2_vec_50 <- 1:m
est1_vec_100 <- 1:m
est2_vec_100 <- 1:m

# Corro los estimadores
set.seed(123)
for (i in seq(1,m)) {
  x_5 <- runif(n = 5, min = 0, max = theta)
  est1_vec_5[i] <- est1(x_5)
  est2_vec_5[i] <- est2(x_5)
}
set.seed(123)
for (i in seq(1,m)) {
  x_30 <- runif(n = 30, min = 0, max = theta)
  est1_vec_30[i] <- est1(x_30)
  est2_vec_30[i] <- est2(x_30)
}
set.seed(123)
for (i in seq(1,m)) {
  x_50 <- runif(n = 50, min = 0, max = theta)
  est1_vec_50[i] <- est1(x_50)
  est2_vec_50[i] <- est2(x_50)
}
set.seed(123)
for (i in seq(1,m)) {
  x_100 <- runif(n = 100, min = 0, max = theta)
  est1_vec_100[i] <- est1(x_100)
  est2_vec_100[i] <- est2(x_100)
}

# Armo un tibble para hacer todo al mismo tiempo
n_vec <- c(rep(5, 2*m), rep(30, 2*m), rep(50, 2*m), rep(100, 2*m))
est_vec <- c(rep("mean", m), rep("max", m), rep("mean", m), rep("max", m), rep("mean", m), rep("max", m), rep("mean", m), rep("max", m))
data <- tibble(n = n_vec,
               est = est_vec,
               value = c(est1_vec_5, est2_vec_5, est1_vec_30, est2_vec_30, est1_vec_50, est2_vec_50, est1_vec_100, est2_vec_100))

# Una figura que muestra la distribución de los estimadores y el tamaño de muestra
data %>% ggplot(aes(x = factor(n), y = value, color = est, fill = est)) +
  geom_hline(yintercept = 3, alpha  = .3) +
  geom_boxplot(alpha = .2) +
  labs(x = "Tamaño de la muestra",
       y = "Valor del estimador",
       color = "Tipo de estimador", 
       fill = "Tipo de estimador") +
  theme_bw() +
  theme(legend.position = "top")

# Ahora calculo sesgo, varianza y error cuadrático medio empíricos.
data_summ <- data %>% group_by(n, est) %>%
  summarise(bias = mean(value)-3,
            var = mean((value-mean(value))^2),
            EMSE = mean((value-3)^2))

## Sesgo ####
### 1 ####
data_summ %>% select(all_of(c("n", "est", "bias"))) %>% filter(est == "mean")

### 2 ####
data_summ %>% select(all_of(c("n", "est", "bias"))) %>% filter(est == "max")

### 3 ####
data_summ %>% ggplot(aes(x = n,
           y = bias,
           color = est)) +
  geom_point() + 
  geom_line() +
  labs(x = "n", y = "Sesgo empírico", color = "Estimador") +
  theme_bw() + 
  scale_color_manual(breaks = c("mean", "max"),
                     values=c("red", "blue")) +
  theme(legend.position = "top")
  
## Varianza ####
### 4 ####
data_summ %>% select(all_of(c("n", "est", "var"))) %>% filter(est == "mean")

### 5 ####
data_summ %>% select(all_of(c("n", "est", "var"))) %>% filter(est == "max")

### 3 ####
data_summ %>% ggplot(aes(x = n,
                         y = var,
                         color = est)) +
  geom_point() + 
  geom_line() +
  labs(x = "n", y = "Varianza empírica", color = "Estimador") +
  theme_bw() + 
  scale_color_manual(breaks = c("mean", "max"),
                     values=c("red", "blue")) +
  theme(legend.position = "top")

## Error cuadrático medio ####
### 7 ####
data_summ %>% select(all_of(c("n", "est", "EMSE"))) %>% filter(est == "mean")

### 8 ####
data_summ %>% select(all_of(c("n", "est", "EMSE"))) %>% filter(est == "max")

### 9 ####
data_summ %>% ggplot(aes(x = n,
                         y = EMSE,
                         color = est)) +
  geom_point(size = 2) + 
  geom_line(size = 1) +
  labs(x = "n", y = "EMSE", color = NULL) +
  scale_x_continuous(breaks = c(5, 30, 50, 100)) +
  scale_color_manual(breaks = c("mean", "max"),
                     values = c("red", "blue"),
                     labels = c(TeX(r"($\hat{\theta}$)"), TeX(r"($\tilde{\theta}$)"))) +
  theme_bw() + 
  theme(legend.position = c(0.87, 0.25),
        legend.text = element_text(size = 20))

