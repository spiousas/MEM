library(dplyr)
library(readr)

# Pollos hermanos ####
data <- read_csv("Documentos/Spiousas/Inferencia/data/pollos-hermanos.csv")

data <- data %>%
  mutate(Z = A-B) 

est <- sqrt(nrow(data)) * abs(mean(data$Z)) / sqrt(1/(nrow(data)-1)*sum((data$Z-mean(data$Z))^2))
est

p_a_mano <- 2*(1-pt(q = est, df = nrow(data)-1))

p_t.test_Z <- t.test(data$Z)
p_t.test_AB <- t.test(data$A, data$B, paired = T)

# Pollos hermanos ####
flujo <- read_csv("Documentos/Spiousas/Inferencia/data/flujo-vehicular.csv")
flujo

t.test(flujo$n19, flujo$n23, paired = T)
