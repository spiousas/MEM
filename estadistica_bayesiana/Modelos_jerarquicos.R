# Load packages
library(bayesrules)
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(broom.mixed)
library(forcats)

# Load data
data(spotify)

# Pooled ####
spotify <- spotify %>% 
  select(artist, title, popularity) %>% 
  mutate(artist = fct_reorder(artist, popularity, .fun = 'mean'))

artist_means <- spotify %>% 
  group_by(artist) %>% 
  summarize(count = n(), popularity = mean(popularity))

artist_means %>%
  slice(1:2, 43:44)

head(artist_means, 2)

artist_means %>% 
  summarize(min(count), max(count))

ggplot(spotify, aes(x = popularity)) + 
  geom_density()

prior_summary()

spotify_complete_pooled <- stan_glm(
  popularity ~ 1, 
  data = spotify, family = gaussian, 
  prior_intercept = normal(50, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 84735)

# Get prior specifications
prior_summary(spotify_complete_pooled)

complete_summary <- tidy(spotify_complete_pooled, 
                         effects = c("fixed", "aux"), 
                         conf.int = TRUE, conf.level = 0.80)
complete_summary

set.seed(84735)
predictions_complete <- posterior_predict(spotify_complete_pooled,
                                          newdata = artist_means)

ppc_intervals(artist_means$popularity, yrep = predictions_complete,
              prob_outer = 0.80) +
  ggplot2::scale_x_continuous(labels = artist_means$artist,
                              breaks = 1:nrow(artist_means)) +
  xaxis_text(angle = 90, hjust = 1)

# No pooled ####
spotify_hierarchical <- stan_glmer(
  popularity ~ (1 | artist), 
  data = spotify, family = gaussian,
  prior_intercept = normal(50, 2.5, autoscale = TRUE),
  prior_aux = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 84735)

# Confirm the prior tunings
prior_summary(spotify_hierarchical)

set.seed(84735)
prediction_shortcut <- posterior_predict(
  spotify_hierarchical,
  newdata = data.frame(artist = c("Frank Ocean", "Mohsen Beats")))

# Posterior predictive model plots
mcmc_areas(prediction_shortcut, prob = 0.8) +
  ggplot2::scale_y_discrete(labels = c("Frank Ocean", "Mohsen Beats"))

set.seed(84735)
predictions_hierarchical <- posterior_predict(spotify_hierarchical, 
                                              newdata = artist_means)

# Posterior predictive plots
ppc_intervals(artist_means$popularity, yrep = predictions_hierarchical, 
              prob_outer = 0.80) +
  ggplot2::scale_x_continuous(labels = artist_means$artist, 
                              breaks = 1:nrow(artist_means)) +
  xaxis_text(angle = 90, hjust = 1) + 
  geom_hline(yintercept = 58.4, linetype = "dashed")

artist_chains <- spotify_hierarchical %>%
  spread_draws(`(Intercept)`, b[,artist]) %>% 
  mutate(mu_j = `(Intercept)` + b) 
artist_chains


spotify_hierarchical_df <- as.data.frame(spotify_hierarchical)
set.seed(84735)
ocean_chains <- spotify_hierarchical_df %>%
  rename(b = `b[(Intercept) artist:Frank_Ocean]`)  %>% 
  select(`(Intercept)`, b, sigma) %>% 
  mutate(mu_ocean = `(Intercept)` + b,
         y_ocean = rnorm(20000, mean = mu_ocean, sd = sigma))

ocean_chains %>% 
  mean_qi(y_ocean, .width = 0.80) 
