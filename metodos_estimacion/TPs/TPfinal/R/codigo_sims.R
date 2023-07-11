# Estimadores de momentos
## Momentos def ####
mu_mo <- function(x) {
  mean(x)
}

r_mo <- function(x) {
  (mean(x))^2/(mean(x^2) - mean(x) - mean(x)^2)
}

## Testeo que anden
data <- rnbinom(n = 1000, size = 1, mu = 5)
mean(data)
var(data)
mu_mo(data)
r_mo(data)

## Asintotico Momentos ####
ns <- c(1e1, 5e1, 1e2, 5e2)
Nrep <- 1e4
mu_pob <- 5
r_pob <- 1

est_rs <- vector(length = length(ns)*Nrep)
est_mus <- vector(length = length(ns)*Nrep)
est_ns <- vector(length = length(ns)*Nrep)

set.seed(12)
for (i in 1:length(ns)) {
  cat(paste("n =", ns[i]), "\n")
  for (j in 1:Nrep) {
    data <- rnbinom(n = ns[i], size = r_pob, mu = mu_pob)
    est_ns[(i-1)*Nrep+ j] <- ns[i]
    est_mus[(i-1)*Nrep + j] <- mu_mo(data)
    est_rs[(i-1)*Nrep + j] <- r_mo(data)
  }
}

sim_mo <- tibble(n = est_ns,
                 mu = est_mus,
                 r = est_rs) %>%
  filter(r!=Inf) %>%
  group_by(n) %>%
  mutate(mu_est = (mu - mu_pob)/sd(mu),
         r_est = (r - r_pob)/sd(r))

sim_mo %>% 
  group_by(n) %>%
  summarise(ECME = mean(r - r_pob)^2)

sim_mo %>%
  ggplot() +
  geom_histogram(aes(x = mu_est, y = ..density..), alpha = .5) + 
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), color = "red", size = 1) +
  scale_x_continuous(limits = c(-3,3)) +
  facet_grid(n~., labeller = label_both) +
  labs(x = "mu de momentos estandarizado (Nrep = 10000)",
       y = "densidad") +
  theme_bw()

sim_mo %>%
  #filter(n==10) %>%
  ggplot() +
  geom_histogram(aes(x = r_est, y = ..density..), alpha = .5) + 
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), color = "red", size = 1) +
  scale_x_continuous(limits = c(-3,3)) +
  facet_grid(n~., labeller = label_both, scales="free_y") +
  labs(x = "r de momentos estandarizado (Nrep = 10000)",
       y = "densidad") +
  theme_bw()

# Estimadores de MV ####
## Testeo que anden
data <- rnbinom(n = 1000, size = 10, mu = 10)
est_mv <- fitdistr(data, densfun = "negative binomial")
est_mv

## Asintotico MV ####
ns <- c(1e1, 1e2, 1e3)
Nrep <- 1e3
mu_pob <- 5
r_pob <- 1

est_rs <- vector(length = length(ns)*Nrep)
est_mus <- vector(length = length(ns)*Nrep)
est_ns <- vector(length = length(ns)*Nrep)

for (i in 1:length(ns)) {
  cat(paste("n =", ns[i]), "\n")
  for (j in 1:Nrep) {
    data <- rnbinom(n = ns[i], size = r_pob, mu = mu_pob)
    est_mv <- fitdistr(data, densfun = "negative binomial")
    est_ns[(i-1)*Nrep+ j] <- ns[i]
    est_mus[(i-1)*Nrep + j] <- est_mv$estimate[2]
    est_rs[(i-1)*Nrep + j] <- est_mv$estimate[1]
  }
}

sim_mv <- tibble(n = est_ns,
                 mu = est_mus,
                 r = est_rs) %>%
  group_by(n) %>%
  mutate(mu_est = (mu - mu_pob)/(sd(mu)),
         r_est = (r - r_pob)/(sd(r))) 

sim_mv %>%
  ggplot(aes(x = mu_est)) +
  geom_histogram() + 
  facet_grid(n~.)

sim_mv %>%
  ggplot(aes(x = r_est)) +
  geom_histogram() + 
  facet_grid(n~.)
