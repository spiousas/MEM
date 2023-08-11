pacman::p_load(here, tidyverse, openintro, corrplot)

# Ejercicio 1 ####
data("bdims")
help("bdims")

## a ####
cor_matrix <- cor(bdims[,1:21])

# Cantidad de correlaciones que tengo que calcular
sum(seq(0,20))

# La mejor manera de exhibir la información es con una matriz de correlación
corrplot(cor_matrix)

# Están todas positivamente correlacionadas

## b ####
# Busco las dos variables con mayor correlación (fuera de la diagonal principal)
indexes <- which(cor_matrix == max(cor_matrix[cor_matrix<1]), arr.ind = TRUE)
rownames(indexes)

bdims %>% 
  dplyr::select(rownames(indexes)) %>%
  ggplot(aes_string(x = rownames(indexes)[1],
                    y = rownames(indexes)[2])) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  labs(title = "Las dos variables menos correlacionadas",
       subtitle = paste0("Las dos variables menos correlacionadas son ",rownames(indexes)[1]," y ",rownames(indexes)[2],
                         " (corr=", round(max(cor_matrix[cor_matrix<1]),digits = 2), ")")) +
  theme_bw()
# Me parece que el número resume adecuadamente el vínculo entre ambas variables

## c ####
# Busco las dos variables con menow correlación (fuera de la diagonal principal)
indexes <- which(cor_matrix == min(cor_matrix[cor_matrix<1]), arr.ind = TRUE)
rownames(indexes)

bdims %>% 
  dplyr::select(rownames(indexes)) %>%
  ggplot(aes_string(x = rownames(indexes)[1],
                    y = rownames(indexes)[2])) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  labs(title = "Las dos variables menos correlacionadas",
       subtitle = paste0("Las dos variables menos correlacionadas son ",rownames(indexes)[1]," y ",rownames(indexes)[2],
                         " (corr=", round(min(cor_matrix[cor_matrix<1]),digits = 2), ")")) +
  theme_bw()
# Me parece que el número resume adecuadamente el vínculo entre ambas variables

## d ####
bdims %>% 
  ggplot(aes(x = hgt, y = wgt)) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  labs(title = "Peso vs. Altura",
       subtitle = paste0("Las correlación vale ", round(cor(bdims$hgt, bdims$wgt), digits = 3))) +
  theme_bw()
# Me parece que el número resume adecuadamente el vínculo entre ambas variables

## e ####
bdims %>% 
  ggplot(aes(x = bia_di, y = age)) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  labs(title = "bia_di vs. age",
       subtitle = paste0("Las correlación vale ", round(cor(bdims$bia_di, bdims$age), digits = 3))) +
  theme_bw()

bdims %>% 
  ggplot(aes(x = bia_di, y = bii_di)) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  labs(title = "bia_di vs. bii_di",
       subtitle = paste0("Las correlación vale ", round(cor(bdims$bia_di, bdims$bii_di), digits = 3))) +
  theme_bw()

bdims %>% 
  ggplot(aes(x = bia_di, y = che_de)) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  labs(title = "bia_di vs. che_de",
       subtitle = paste0("Las correlación vale ", round(cor(bdims$bia_di, bdims$che_de), digits = 3))) +
  theme_bw()

bdims %>% 
  ggplot(aes(x = bia_di, y = wri_di)) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  labs(title = "bia_di vs. wri_di",
       subtitle = paste0("Las correlación vale ", round(cor(bdims$bia_di, bdims$wri_di), digits = 3))) +
  theme_bw()


