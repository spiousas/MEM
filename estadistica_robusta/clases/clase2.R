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
min_n <- 1
max_n <- 1e3
step <- 20
nrep <- 4000

set.seed(123)
x <- matrix(rnorm(nrep * max_n, 1, 1), nrep, max_n)

eficiencia_mediana <- expand_grid(rep = 1:nrep,
                                  ns  = seq(min_n, max_n, step)) |>
  rowwise() |>
  mutate(mean   = mean  (x[rep, 1:ns]),
         median = median(x[rep, 1:ns])) |>
  group_by(ns) |>
  summarise(eficiencia = var(mean) / var(median),
            coc_medias = mean(mean) / mean(median))

write_csv(eficiencia_mediana, here("estadistica_robusta/entrega/data/eficiencias.csv"))

eficiencia_mediana_long <- eficiencia_mediana |>
  rename(`Cociente de varianzas (eficiencia)` = eficiencia,
         `Cociente de medias` = coc_medias) |>
  pivot_longer(cols = `Cociente de varianzas (eficiencia)`:`Cociente de medias`, 
               names_to = "medida", 
               values_to = "valor")

eficiencia_mediana_long |> 
  ggplot(aes(x = ns, y = valor, color = medida)) +
  geom_hline(yintercept = 2/pi, linetype = "dashed") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Tamaño de la muestra (n)", y = "Cocientes", color = NULL) +
  theme_bw() +
  geom_magnify(from = c(250, 900, .6, 1.4), to = c(250, 900, -8, -1)) +
  theme(legend.position = "bottom")

# Eficiencia de la media alpha podada ####
min_n <- 1
max_n <-1e3
step <- 20
nrep <- 4000

set.seed(1234)
x <- matrix(rnorm(nrep * max_n, 0, 1), nrep, max_n)

eficiencia_podada <- expand_grid(rep = 1:nrep,
                                 ns  = seq(min_n, max_n, step)) %>%
  rowwise() %>%
  mutate(mean   = mean(x[rep, 1:ns]),
         median = mean(x[rep, 1:ns], trim = .25)) %>%
  group_by(ns) %>%
  summarise(eficiencia = var(mean) / var(median))

eficiencia_podada %>% ggplot(aes(x = ns, y = eficiencia)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = .83, linetype = "dashed") +
  theme_bw()
