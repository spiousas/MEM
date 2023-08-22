pacman::p_load(here, tidyverse, matlib, factoextra)

# Ejercicio 2.3 ####
data_constructora <- read_delim(here("visualización/TPs/data/constructora.txt")
                                , delim = " ")

## a ####
data_constructora_center <-
  data_constructora %>% 
  mutate(x1 = x1 - mean(x1),
         x2 = x2 - mean(x2),
         x2 = x3 - mean(x3))
         
data_constructora_center %>% ggplot(aes(x = x1,
                                        y = x2)) +
  geom_point() + theme_bw()

data_constructora_center %>% ggplot(aes(x = x1,
                                        y = x3)) +
  geom_point() + theme_bw()

data_constructora_center %>% ggplot(aes(x = x2,
                                        y = x3)) +
  geom_point() + theme_bw()
  
## b ####
a <- eigen(cov(as.matrix(data_constructora_center[,2:4])))$vectors[,1:2]
a

as.matrix(data_constructora_center[,2:4]) %*% a

data_pca <- as_tibble(as.matrix(data_constructora_center[,2:4]) %*% a, .name_repair = "universal") %>% 
  rename(x1 = "...1", x2 = "...2") 
data_pca

## c ####
data_pca %>% ggplot(aes(x = x1,
                        y = x2)) +
  geom_point(color = "grey") + 
  theme_bw() + theme(aspect.ratio=1)

## d ####
# La matriz de cov de las primeras dos PC
cov(data_pca)
# La matriz de cov de las tres PC
cov(as.matrix(data_constructora_center[,2:4]) %*% eigen(cov(as.matrix(data_constructora_center[,2:4])))$vector)

# Ambas matrices son casi iguales porque la PC3 no explica nada de varianza (autovalor = 0)

sum(eigen(cov(as.matrix(data_constructora_center[,2:4])))$values[1:2]) / sum(eigen(cov(as.matrix(data_constructora_center[,2:4])))$values)
# Las dos primeras componentes explican el 100% de la variabilidad

## e ####
# NO ENTIENDO QUÉ TENGO QUE HACER ACÁ

# Ejercicio 2.4 ####
data_paises <- read_csv(here("visualización/TPs/data/paises_mundo.csv"))

# Los scatterplot de a pares de columnas
pairs(data_paises[2:6])

## Version i. - Sin scale ni transformación ####
# Calculo el PCA
pca_paises <- prcomp(as.matrix(data_paises[2:6]), scale = F)
pca_paises
summary(pca_paises)

# Centrar x a lo bestia
# centered_x <- as.matrix(data_paises[,2:6])
# for (i in 1:nrow(data_paises)) {
#   centered_x[i,] <- as.matrix(data_paises[i,2:6]) - colMeans(data_paises[2:6])
# }

# Los scores son pca_nci_paises$x o también los puedo calcular como:
centered_x <- as.matrix(data_paises[2:6]) - colMeans(data_paises[2:6])[col(data_paises[2:6])]
scores <- centered_x %*% pca_paises$rotation 
plot(scores)
plot(pca_paises$x)

fviz_pca_biplot(pca_paises,
                label = "var",
                col.ind = "black",
                size.ind = .1)

eigs <- pca_paises$sdev^2

rbind(
  SD = sqrt(eigs),
  Percentage = eigs/sum(eigs)*100,
  Cumulative = cumsum(eigs)/sum(eigs)*100)

heatmap(summary(pca_paises)$rotation,  Colv = NA, Rowv = NA, scale="column")

## Version ii. - Sin scale y transformando con log ####
# Calculo el PCA
log_data_paises <- log(as.matrix(data_paises[2:6]))
pca_log_paises <- prcomp(log_data_paises, scale = F)
pca_log_paises
summary(pca_log_paises)

# Los scores son pca_nci_paises$x o también los puedo calcular como:
centered_x <- log_data_paises - colMeans(log_data_paises)[col(log_data_paises)]
scores <- centered_x %*% pca_log_paises$rotation 
plot(scores)
plot(pca_log_paises$x)

fviz_pca_biplot(pca_log_paises,
                label = "var",
                col.ind = "black",
                size.ind = .1)

eigs <- pca_log_paises$sdev^2

rbind(
  SD = sqrt(eigs),
  Percentage = eigs/sum(eigs)*100,
  Cumulative = cumsum(eigs)/sum(eigs)*100)

heatmap(summary(pca_log_paises)$rotation,  Colv = NA, Rowv = NA, scale="column")

## Version iii. - Sin transformar pero scaleando los datos ####
# Calculo el PCA
pca_paises_scaled <- prcomp(as.matrix(data_paises[2:6]), scale = T)
pca_paises_scaled
summary(pca_paises_scaled)

# Los scores son pca_nci_paises$x o también los puedo calcular como:
scaled_x <- scale(as.matrix(data_paises[2:6]))
scores <- scaled_x %*% pca_paises_scaled$rotation 
plot(scores)
plot(pca_paises_scaled$x)

fviz_pca_biplot(pca_paises_scaled,
                label = "var",
                col.ind = "black",
                size.ind = .1)

eigs <- pca_paises_scaled$sdev^2

rbind(
  SD = sqrt(eigs),
  Percentage = eigs/sum(eigs)*100,
  Cumulative = cumsum(eigs)/sum(eigs)*100)

heatmap(summary(pca_paises_scaled)$rotation,  Colv = NA, Rowv = NA, scale="column")

## b ####
# Pareciera que los outliers sí afectan al PCA, pero la verdad es que no estoy seguro CONSUILTAR

# Ejercicio 2.5 ####

data_vinos <- read_csv(here("visualización/TPs/data/vinos.csv"))

pca_nci_vinos <- prcomp(as.matrix(data_vinos), scale = T)
biplot(pca_nci_vinos)

# Voy a usar la función fviz_pca_biplot del paquete {factoextra}
fviz_pca_biplot(pca_nci_vinos,
                label = "var",
                col.ind = "gray80",
                size.ind = .1)
