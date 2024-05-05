# HACER

library(tidyverse)
library(ggplot2)

rm(list = ls())

n_muestra <- 200000
n_rep <- 1000

data = matrix(rnorm(n_muestra*n_rep), nrow=n_rep, ncol=n_muestra)

my_sim <- 
  expand_grid(id = 1:n_rep,
              n = seq(10, n_muestra, 1000)) %>%
  rowwise() %>%
  mutate(
    mean   = mean  (data[id, 1:n]),
    median = median(data[id, 1:n]))

my_res <- my_sim %>% 
  group_by(n) %>%
  summarise(
    v_mean   = var(mean),
    v_median = var(median))
  
my_res %>%
  ggplot(aes(x=n, y=v_mean/v_median)) + 
  geom_line() +
  geom_hline(yintercept=2/pi) +
  theme_bw()

  