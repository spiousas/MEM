# Primero voy a ver la función de densidad conjunta:
library(tidyverse)

x <- seq(0,1,.1)
y <- seq(0,1,.1)

data <- expand_grid(x,y) %>%
  mutate(f = 2.4 * exp(-x) * exp(-2.4*y))
data %>% ggplot(aes(x = x,
           y = y,
           fill = f)) +
  geom_tile()
  
# b ####
ylim <- 0.42
yexp <- -2.4
exp(yexp*ylim) - exp(0)

xlim <- 1
xexp <- 1
exp(-Inf) - exp(-1)

(exp(yexp*ylim) - exp(0)) * (exp(-Inf) - exp(-1))
