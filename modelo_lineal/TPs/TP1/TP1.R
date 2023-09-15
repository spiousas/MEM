pacman::p_load(here, tidyverse, openintro, corrplot, datasauRus)

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

# Ejercicio 4 ####
data("datasaurus_dozen")
plot(datasaurus_dozen[,c(2,3)])
cor(datasaurus_dozen[,c(2,3)])

categoria <- unique(datasaurus_dozen$dataset)
categoria

## a ####
categoria_i <- 1
plot(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],c(2,3)], 
     main = paste("datasaurus dozen", categoria[categoria_i]),
     pch = 20)

cor(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],c(2,3)])
colMeans(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],c(2,3)])
var(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],2])
var(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],3])

## b ####
categoria_i <- 6
plot(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],c(2,3)], 
     main = paste("datasaurus dozen", categoria[categoria_i]),
     pch = 20)

cor(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],c(2,3)])
colMeans(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],c(2,3)])
var(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],2])
var(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],3])

## c ####
categoria_i <- 9
plot(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],c(2,3)], 
     main = paste("datasaurus dozen", categoria[categoria_i]),
     pch = 20)

cor(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],c(2,3)])
colMeans(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],c(2,3)])
var(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],2])
var(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],3])

## d ####
categoria_i <- 11
plot(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],c(2,3)], 
     main = paste("datasaurus dozen", categoria[categoria_i]),
     pch = 20)

cor(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],c(2,3)])
colMeans(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],c(2,3)])
var(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],2])
var(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],3])

## e ####
categoria_i <- 5
plot(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],c(2,3)], 
     main = paste("datasaurus dozen", categoria[categoria_i]),
     pch = 20)

cor(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],c(2,3)])
colMeans(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],c(2,3)])
var(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],2])
var(datasaurus_dozen[datasaurus_dozen[,1]==categoria[categoria_i],3])

## f ####
if (require(ggplot2)) {
  ggplot(datasaurus_dozen, aes(x = x, y = y, colour = dataset)) +
    geom_point() +
    theme_void() +
    theme(legend.position = "none") +
    facet_wrap(~dataset, ncol = 3)
}

# Ejercicio 5 ####
data <- anscombe

## a ####
plot(data[,1], data[,5])
plot(data[,2], data[,6])
plot(data[,3], data[,7])
plot(data[,4], data[,8])

## b ####
# Para el primer par
mean(data[,1]) 
mean(data[,5])
sd(data[,1]) 
sd(data[,5])
cor(data[,1], data[,5])

# Para el segundo par
mean(data[,2]) 
mean(data[,6])
sd(data[,2]) 
sd(data[,6])
cor(data[,2], data[,6])

# Para el tercer par
mean(data[,3]) 
mean(data[,7])
sd(data[,3]) 
sd(data[,7])
cor(data[,3], data[,7])

# Para el cuarto par
mean(data[,4]) 
mean(data[,8])
sd(data[,4]) 
sd(data[,8])
cor(data[, 4], data[,8])

# A pesar de que los scatter plots son tan diferentes, las medias, los desvíos,
# y las correlaciones son igualesx

# Ejercicio 7
n <- 40

## a ####
X <- rexp(n = 40, rate = 1)
b0 <- 5
b1 <- -2
Y <- b0 + b1*X + rnorm(n = 40, mean = 0, sd = sqrt(3))

plot(X, Y)

## b ####
X <- rexp(n = 40, rate = 1)
b0 <- 5
b1 <- -2
Y <- b0 + b1*X + runif(n = 40, min = -3, max = 3)

plot(X, Y)

## c ####
X <- rexp(n = 40, rate = 1)
b0 <- 5
b1 <- -2
Y <- b0 + b1*X + rgamma(n = 40, shape = .4, rate = .01) + 30

plot(X, Y)

## d ####
X <- rexp(n = 40, rate = 1)
b0 <- 5
b1 <- -2
Y <- b0 + b1*X + rnorm(n = 40, mean = 0, sd = sqrt(25))

plot(X, Y)
