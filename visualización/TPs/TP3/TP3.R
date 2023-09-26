pacman::p_load(here, tidyverse, MASS, HSAUR2, plotly, factoextra, fpc, janitor, StatMatch)

# Ejercicio 3.1 ####
## a ####

data_planets <- HSAUR2::planets %>% scale(.)
colMeans(data_planets)
cov(data_planets)

# Uso K-medias
set.seed(123)
k <- 4
planets_clusters <- kmeans(x = data_planets, centers = k)
planets_clusters

# planets_clusters tiene:
#   - size: El tamaño de cada cluster.
#   - centers: Los centros de cada cluster.
#   - withinss: La suma de cuadrados dentro de cada cluster.
#   - tot.withinss: La varianza total de los clusters (lo que quiero minimizar),
#   - betweenss: La suma de cuadrados between.
#   - cluster: Un vector que te dice a qué cluster pertenece cada observación.
data_planets_cluster <- data_planets %>%
  bind_cols(planets_clusters$cluster) %>%
  rename(cluster = `...4`)

# Le pongo el número de cluster y voy a calcular los centros y las sumas de cuadrados
# a ver si me dan lo mismo
data_planets_cluster

planets_center <- data_planets_cluster %>%
  group_by(cluster) %>% 
  mutate(n = n()) %>%
  group_by(cluster, n) %>% 
  summarise_all(mean)
  
planets_center
planets_clusters$centers
planets_clusters$size
# Dan igual!

planets_withinss <- data_planets_cluster %>%
  group_by(cluster) %>%
  summarise(SS = sum((mass-mean(mass))^2 + (period-mean(period))^2 + (eccen-mean(eccen))^2))

planets_withinss
planets_clusters$withinss
# Dan igual!

planets_withinss %>% summarise(total_SS = sum(SS))
planets_clusters$tot.withinss
# Dan igual!

planets_totalss <- data_planets_cluster %>%
  ungroup() %>%
  summarise(SS = sum(((mass-mean(mass))^2 + (period-mean(period))^2 + (eccen-mean(eccen))^2)))

planets_totalss
planets_clusters$totss
# Dan igual!

planets_betweenss <- planets_center %>%
  ungroup() %>%
  summarise(SS = sum(n * ((mass-mean(data_planets_cluster$mass))^2 + (period-mean(data_planets_cluster$period))^2 + (eccen-mean(data_planets_cluster$eccen))^2)))
# La suma de cuadrados between es al centro de los datos (el equivalente al promedio pesado de los centros)
# y se pesa por el n de cada cluster

planets_betweenss
planets_clusters$betweenss
# Dan igual!

# Vamos a ver los clusters
# En 3D 
plot_ly(data_planets_cluster,x = ~mass, y = ~period, z = ~eccen, color = ~as.factor(data_planets_cluster$cluster)) %>% 
  add_markers()

# En 2D 
PCA_planets <- prcomp(as.matrix(data_planets, scale = F))
summary(PCA_planets)

as.matrix(data_planets) %*% PCA_planets$rotation
# Esto es lo mismo que PCA_planets$x

planets_PCA_cluster <- as_tibble(PCA_planets$x) %>%
  bind_cols(planets_clusters$cluster) %>%
  rename(cluster = `...4`) 

planets_PCA_cluster %>%
  ggplot(aes(x = PC1,
             y = PC2,
             color = factor(cluster))) +
  geom_point() +
  theme_bw() +
  labs(color = "# Cluster") +
  theme(legend.position = "top")

# Con funciones de factoextra
fviz_cluster(object = planets_clusters, data = data_planets, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco") +
  theme_bw() +
  theme(legend.position = "none")
    
## b ####
ks <- 1:15
WSCC <- tibble(k = ks, WSCC = NA)
for (i in 1:length(ks)) {
  WSCC$WSCC[WSCC$k == ks[i]] <- kmeans(x = data_planets, centers = ks[i])$tot.withinss
}

WSCC %>% ggplot(aes(x = k,
                    y = WSCC)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = ks) +
  theme_bw()
# No pareciera haber un codo en el WSS

# Usando funciones de factoExtra
# WCSS
fviz_nbclust(x = data_planets, FUNcluster = kmeans, method = "wss", k.max = 15,
             diss = dist(data_planets, method = "euclidean"))

# Silhouette
fviz_nbclust(x = data_planets, FUNcluster = kmeans, method = "silhouette", k.max = 15) +
  labs(title = "Numero Optimo de clusters")

# GAP
# Sin estandarizar
fviz_nbclust(x = HSAUR2::planets, FUNcluster = kmeans, method = "gap_stat", nboot = 500,
             k.max = 15, verbose = FALSE, nstart = 50) +labs(title = "Número óptimo de clusters")

# Estandarizando
fviz_nbclust(x = HSAUR2::planets, FUNcluster = kmeans, method = "gap_stat", nboot = 500,
             k.max = 15, verbose = FALSE, nstart = 50) +labs(title = "Número óptimo de clusters")

# Ejercicio 3.2 ####
## a ####
data_credit_card <- read_csv(here("visualización/TPs/TP3/data/credit-card-data.csv")) %>%
  select(all_of(c("oneoff_purchases", "install_purchases", "cash_adv", "oneoff_purchases_freq",
                  "install_purchases_freq", "cash_adv_freq", "purchases_trx", "credit_limit", "prc_full_payment"))) %>%
  drop_na() %>%
  scale(.)

ks <- 1:10
WSCC <- tibble(k = ks, WSCC = NA)
for (i in 1:length(ks)) {
  WSCC$WSCC[WSCC$k == ks[i]] <- kmeans(x = data_credit_card, centers = ks[i])$tot.withinss
}

WSCC %>% ggplot(aes(x = k,
                    y = WSCC)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = ks) +
  theme_bw()

# set.seed(123)
# fviz_nbclust(x = data_credit_card, FUNcluster = kmeans, method = "wss", k.max = 10, nstart = 50)
# Esto tarda mil años

# La verdad es que no hay un codo fácilmente identificable

set.seed(123)
credit_card_clusters <- kmeans(x = data_credit_card, centers = 5, nstart = 50)
fviz_cluster(object = credit_card_clusters, data = data_credit_card, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco") +
  theme_bw() +
  theme(legend.position = "none")

## b ####
# Ahora ajustamos DBScan con 
set.seed(123)
credit_card_dbscan <- fpc::dbscan(data = data_credit_card, eps = 0.25, MinPts = 15)
sum(credit_card_dbscan$cluster==0)/nrow(data_credit_card)
max(credit_card_dbscan$cluster)
# El 78.8% de los puntos se considera ruido y hay 9 clusters

set.seed(123)
credit_card_dbscan <- fpc::dbscan(data = data_credit_card, eps = 0.5, MinPts = 15)
sum(credit_card_dbscan$cluster==0)/nrow(data_credit_card)
max(credit_card_dbscan$cluster)
# El 47.4% de los puntos se considera ruido y hay 3 clusters

set.seed(123)
credit_card_dbscan <- fpc::dbscan(data = data_credit_card, eps = 0.35, MinPts = 15)
sum(credit_card_dbscan$cluster==0)/nrow(data_credit_card)
max(credit_card_dbscan$cluster)
# El 65% de los puntos se considera ruido y hay 5 clusters

set.seed(123)
credit_card_dbscan <- fpc::dbscan(data = data_credit_card, eps = 0.45, MinPts = 15)
sum(credit_card_dbscan$cluster==0)/nrow(data_credit_card)
max(credit_card_dbscan$cluster)
# El 52.5% de los puntos se considera ruido y hay 4 clusters

credit_card_data_dbscan <- as_tibble(as.matrix(data_credit_card) %*% prcomp(data_credit_card)$rotation[,1:3]) %>%
  bind_cols(credit_card_dbscan$cluster) %>%
  rename(cluster = `...4`) %>%
  filter(cluster !=0)

plot_ly(credit_card_data_dbscan, x = ~PC1, y = ~PC2, z = ~PC3, color = ~as.factor(cluster)) %>% 
  add_markers()

## c ####
data_credit_card_redux <- as_tibble(as.matrix(data_credit_card) %*% prcomp(data_credit_card)$rotation[,1:3])

set.seed(123)
credit_card_dbscan <- fpc::dbscan(data = data_credit_card_redux, eps = 0.1, MinPts = 15)
sum(credit_card_dbscan$cluster==0)/nrow(data_credit_card)
max(credit_card_dbscan$cluster)

credit_card_data_dbscan <- data_credit_card_redux %>%
  bind_cols(credit_card_dbscan$cluster) %>%
  rename(cluster = `...4`) %>%
  filter(cluster !=0)

plot_ly(credit_card_data_dbscan, x = ~PC1, y = ~PC2, z = ~PC3, color = ~as.factor(cluster)) %>% 
  add_markers()
# La verdad que me parece difícil entender si esto es o no una mejor clusterización

# Ejercicio 3.3 ####
## a ####
data_facebook <- readxl::read_excel(here("visualización/TPs/TP3/data/facebook.xlsx"))
data_facebook

X <- t(as.matrix(data_facebook))
# Como quiero calcular las distancias entre las palabras voy a calcular la distancia
# sobre la traspuesta
  
distancias_canberra <- dist(X, method="canberra", diag=T, upper=T)

## b ####
hc <- hclust(distancias_canberra, method = "ward.D2")
plot(hc)
# Me parecen razonables 7 grupos
rect.hclust(hc, k=7, border="red")

fviz_dend(x = hc, k = 7, cex = 0.6) +
  geom_hline(yintercept = 190, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia Canberra, ward.D2, K=7") +
  theme_minimal()

# El grupo de cada palabra
groups <- cutree(hc, k=7) 

## c ####
# Escalado multidimensional clásico siguiendo el script de Pedro
distancias <- as.matrix(distancias_canberra)
A <- -0.5*distancias^2
n <- dim(distancias)[1]
H <- diag(rep(1,n)) - n^{-1} * (rep(1,n)) %*% t(rep(1,n))
B <- H*A*H
DEL <- eigen(B)$values
round(eigen(B)$values,3)
V <- eigen(B)$vectors

round(B - V%*%diag(DEL)%*%t(V))
# La descomposición es igual a B (esperable)

V1 <- V[,1:2]
t(V1) %*% (V1)
DEL1 <- diag(DEL[1:2])
Y <- V1%*%sqrt(DEL1)
colnames(Y) <- c("C1", "C2")

MDS <- as_tibble(Y) %>%
  mutate(name = rownames(distancias),
         grupo = groups)
  
MDS %>% ggplot(aes(x = C1,
                   y = C2,
                   color = factor(grupo))) +
  geom_text(aes(label = name)) +
  theme_bw() +
  labs(color = "Grupo:") +
  theme(legend.position = "top")

# Está bastante bien, voy a probar ahora con 3
V1 <- V[,1:3]
t(V1) %*% (V1)
DEL1 <- diag(DEL[1:3])
Y <- V1%*%sqrt(DEL1)
colnames(Y) <- c("C1", "C2", "C3")

MDS <- as_tibble(Y) %>%
  mutate(name = rownames(distancias),
         grupo = groups)

plot_ly(MDS, x = ~C1, y = ~C2, z = ~C3, color = ~as.factor(grupo)) %>% 
  add_markers()
# Se separan los grupos que estaban confusos

# Ahora vamos a probar cmdscale
MDS <- as_tibble(cmdscale(distancias_canberra, k = 2, eig = FALSE, add = FALSE, x.ret = FALSE))  %>%
  mutate(name = rownames(distancias),
         grupo = groups)

MDS %>% ggplot(aes(x = V1,
                   y = V2,
                   color = factor(grupo))) +
  geom_text(aes(label = name)) +
  theme_bw() +
  labs(color = "Grupo:") +
  theme(legend.position = "top")

# Está girado pero da lo mismo.

## d ####

distancias_euclidea<- dist(X, method="euclidean", diag=T, upper=T)

hc <- hclust(distancias_euclidea, method = "ward.D2")
plot(hc)
# Me parecen razonables 7 grupos
rect.hclust(hc, k=7, border="red")

fviz_dend(x = hc, k = 7, cex = 0.6) +
  geom_hline(yintercept = 45, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia Canberra, ward.D2, K=7") +
  theme_minimal()

# El grupo de cada palabra
groups <- cutree(hc, k=7) 

## c ####
# Repito todo con euclidea
# Escalado multidimensional clásico siguiendo el script de Pedro
distancias <- as.matrix(distancias_euclidea)
A <- -0.5*distancias^2
n <- dim(distancias)[1]
H <- diag(rep(1,n)) - n^{-1} * (rep(1,n)) %*% t(rep(1,n))
B <- H*A*H
DEL <- eigen(B)$values
round(eigen(B)$values,3)
V <- eigen(B)$vectors

round(B - V%*%diag(DEL)%*%t(V))
# La descomposición es igual a B (esperable)

V1 <- V[,1:2]
t(V1) %*% (V1)
DEL1 <- diag(DEL[1:2])
Y <- V1%*%sqrt(DEL1)
colnames(Y) <- c("C1", "C2")

MDS <- as_tibble(Y) %>%
  mutate(name = rownames(distancias),
         grupo = groups)

MDS %>% ggplot(aes(x = C1,
                   y = C2,
                   color = factor(grupo))) +
  geom_text(aes(label = name)) +
  theme_bw() +
  labs(color = "Grupo:") +
  theme(legend.position = "top")

# Con distancia euclidea da mucho peor

# Está bastante bien, voy a probar ahora con 3
V1 <- V[,1:3]
t(V1) %*% (V1)
DEL1 <- diag(DEL[1:3])
Y <- V1%*%sqrt(DEL1)
colnames(Y) <- c("C1", "C2", "C3")

MDS <- as_tibble(Y) %>%
  mutate(name = rownames(distancias),
         grupo = groups)

plot_ly(MDS, x = ~C1, y = ~C2, z = ~C3, color = ~as.factor(grupo)) %>% 
  add_markers()

# Ejercicio 3.5 ####
## a ####
data_indumentaria <- readxl::read_excel(here("visualización/TPs/TP3/data/indumentaria.xlsx")) %>%
  clean_names()
data_indumentaria

X_indumentaria <- data_indumentaria %>%
  select_if(is.numeric) %>%
  drop_na() %>%
  as.matrix(.)
X_indumentaria

dist_jaccard <- function(data) {
  s <- matrix(nrow = nrow(data), ncol = ncol(data))
  
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      if (i == j) {
        s[i,j] <- 1
      } else {
        s[i,j] <- data[i,j] / (data[i,j] + sum(data[i,]) - data[i,j] + sum(data[,j]) - data[i,j])
      }
    }  
  }
  
  s
}

dist_jaccard <- dist_jaccard(X_indumentaria)
sim <- 1 - dist_jaccard

## b ####
MDS <- as_tibble(cmdscale(sim, k = 2, eig = FALSE, add = FALSE, x.ret = FALSE))  %>%
  mutate(name = data_indumentaria$x1)

MDS %>% ggplot(aes(x = V1,
                   y = V2)) +
  geom_text(aes(label = name)) +
  theme_bw() +
  labs(color = "Grupo:") +
  theme(legend.position = "top")

## c ####
ks <- 1:10
WSCC <- tibble(k = ks, WSCC = NA)
for (i in 1:length(ks)) {
  WSCC$WSCC[WSCC$k == ks[i]] <- kmeans(x = MDS[,1:2], centers = ks[i])$tot.withinss
}

WSCC %>% ggplot(aes(x = k,
                    y = WSCC)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = ks) +
  theme_bw()

# El k óptimo está en 4
cluster_indumentaria <- kmeans(x = MDS[,1:2], centers = 4)

MDS <- MDS %>%
  bind_cols(cluster_indumentaria$cluster) %>%
  rename(cluster = `...4`)

MDS %>% ggplot(aes(x = V1,
                   y = V2,
                   color = as.factor(cluster))) +
  geom_text(aes(label = name)) +
  theme_bw() +
  labs(color = "Grupo:") +
  theme(legend.position = "top")

# En dos dimensiones se ve perfectamente bien

# Ejercicio 3.5 ####
## a ####
data_properties <- read_csv(here("visualización/TPs/TP3/data/properties.csv"))

gower_dist_properties <- gower.dist(as.data.frame(data_properties))
gower_dist_properties

# Gower sirve porque tenemos variables de distinto tipo

dist(as.data.frame(data_properties), method="euclidean", diag=T, upper=T)
# Euclidean no anda por la columna Type

## b ####
# En dos dimensiones
MDS <- as_tibble(cmdscale(gower_dist_properties, k = 2, eig = FALSE, add = FALSE, x.ret = FALSE))

MDS %>% ggplot(aes(x = V1,
                   y = V2)) +
  geom_point() +
  theme_bw() +
  labs(color = "Grupo:") +
  theme(legend.position = "top")

# En tres dimensiones
MDS <- as_tibble(cmdscale(gower_dist_properties, k = 3, eig = FALSE, add = FALSE, x.ret = FALSE))

plot_ly(MDS, x = ~V1, y = ~V2, z = ~V3) %>% 
  add_markers()

## c ####
# Cambiemos los pesos
gower_dist_properties <- gower.dist(as.data.frame(data_properties), var.weights = c(0.2,0.2,0.3,0.1,0.1,0.1))

MDS <- as_tibble(cmdscale(gower_dist_properties, k = 2, eig = FALSE, add = FALSE, x.ret = FALSE))

MDS %>% ggplot(aes(x = V1,
                   y = V2)) +
  geom_point() +
  theme_bw() +
  labs(color = "Grupo:") +
  theme(legend.position = "top")
