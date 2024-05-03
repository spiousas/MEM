# MD ####
my.md <- function(x) {
  mean(abs(x-mean(x)))
}

md <- c()
for (i in 10:10000) {
  set.seed(3)
  md <- c(md, my.md(rnorm(i)))
}

simul_md <- tibble(n = 10:10000, md)

simul_md %>% ggplot(aes(x = n, y = md)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = 2/sqrt(2*pi), linetype = "dashed") +
  theme_bw()

# IQR ####
my.iqr <- function(x) {
  # Corregir el tema de no usar quantile
  quantile(x, probs = .75) - quantile(x, probs = .25)
}

iqr <- c()
for (i in 10:10000) {
  set.seed(3)
  iqr <- c(iqr, my.iqr(rnorm(i)))
}

simul_iqr <- tibble(n = 10:10000, iqr)

simul_iqr %>% ggplot(aes(x = n, y = iqr)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = 2*qnorm(3/4), linetype = "dashed") +
  theme_bw()

# Eficiencia de la mediana ####
ns <- seq(20,2000,10)
nrep <- 1000
eficiencia <- c()

set.seed(3)
for (i in 1:length(ns)) {
  x <- matrix(rnorm(nrep * ns[i], 0, 1), nrep, ns[i])
  aux_media <- apply(x,1,mean)
  aux_mediana <- apply(x,1,median)
  eficiencia <- c(eficiencia, var(aux_media)/var(aux_mediana))
}

simul_eficiencia_mediana <- tibble(n = ns, eficiencia)

simul_eficiencia_mediana %>% ggplot(aes(x = ns, y = eficiencia)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = 0.57, linetype = "dashed") +
  theme_bw()

# Eficiencia de la media alpha podada ####
ns <- seq(20,2000,10)
nrep <- 1000
eficiencia <- c()

set.seed(3)
for (i in 1:length(ns)) {
  print(ns[i])
  x <- matrix(rnorm(nrep * ns[i], 0, 1), nrep, ns[i])
  aux_media <- apply(x,1,mean)
  
  aux_media_podada <- apply(x,1,mean, trim = .25)
  eficiencia <- c(eficiencia, var(aux_media)/var(aux_media_podada))
}

simul_eficiencia_podada <- tibble(n = ns, eficiencia)

simul_eficiencia_podada %>% ggplot(aes(x = ns, y = eficiencia)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = 0.83, linetype = "dashed") +
  theme_bw()
