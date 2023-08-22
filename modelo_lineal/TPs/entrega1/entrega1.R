pacman::p_load(tidyverse, here)

data <- read_csv(here("modelo_lineal/data/simple.csv"))
data
n <- nrow(data)

m1 <- data %>% lm(data = ., salario ~ nota)
m1

m1$fitted.values[21]
m1$fitted.values[23]

sigma <- summary(m1)$sigma
sigma

sd_beta1 <- sigma/sqrt((n-1)*sd(data$nota)^2)
sd_beta1