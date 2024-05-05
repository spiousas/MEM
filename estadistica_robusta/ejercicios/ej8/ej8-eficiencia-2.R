# HACER

library(tidyverse)
library(ggplot2)

rm(list = ls())

n_rep <- 1000

my_sim <- 
  expand_grid(n = c(round(10^seq(0.5,2,by=0.25)),
                    round(10^seq(2.5,4,by=0.5)),
                    100000),
              id = 1:n_rep) %>%
  rowwise() %>%
  mutate(
    mean   = mean  (rnorm(n)),
    median = median(rnorm(n)))


my_res <- my_sim %>% 
  group_by(n) %>%
  summarise(
    m_mean   = mean(mean),
    m_median = mean(median),
    v_mean   = var(mean),
    v_median = var(median))

# varianza
my_res %>%
  ggplot(aes(x=n, y=v_mean/v_median)) + 
  geom_line() +
  geom_hline(yintercept=2/pi, linetype="dashed") +
  scale_x_continuous(trans="log10") +
  ylab("Eficiencia") +
  theme_bw()

# media
my_res %>%
  ggplot(aes(x=n, y=abs(m_mean/m_median))) + 
  geom_line() +
  geom_hline(yintercept=1, linetype="dashed") +
  scale_x_continuous(trans="log10") +
  ylab("Eficiencia") +
  theme_bw()
