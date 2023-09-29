pacman::p_load(here, tidyverse, MASS, plotly, GGally, smacof)

ciudades_tbl <- read_delim(here("visualización/TPs/TP4/data/ciudades.txt"))
ciudades_tbl

# A mano ####
ciudades <- as.matrix(ciudades_tbl[,-1])
ciudades 

A <- -0.5*ciudades^2
n <- dim(ciudades)[1]
H <- diag(rep(1,n))-n^{-1}*(rep(1,n))%*%t(rep(1,n))
B <- H %*% A %*% H
DEL <- eigen(B)$values
round(eigen(B)$values,3)
V <- eigen(B)$vectors

round(B-V%*%diag(DEL)%*%t(V))

## Proyecto en R2 ####
V1<-V[,1:2]
t(V1)%*%(V1)
DEL1<-diag(DEL[1:2])
Y<-V1%*%sqrt(DEL1)
colnames(Y) <- c("C1", "C2")

MDS <- as_tibble(Y) %>%
  mutate(name = ciudades_tbl$...1)

MDS %>% ggplot(aes(x = C1,
                   y = C2)) +
  geom_text(aes(label = name)) +
  theme_bw() +
  labs(color = "Grupo:") +
  theme(legend.position = "top") +
  theme_void()

## Stress en R2 ####
dist_reconstruida <- as.matrix(dist(Y))

p_ij = dist_reconstruida[upper.tri(dist_reconstruida)]
d_ij = ciudades[upper.tri(ciudades)]
denominator = sum(p_ij^2)
nominator = sum((d_ij - p_ij)^2)
normalized_stress = sqrt(nominator/denominator)
normalized_stress

## Proyecto en R3 ####
V1 <- V[,1:3]
t(V1) %*% (V1)
DEL1 <- diag(DEL[1:3])
Y <- V1%*%sqrt(DEL1)
colnames(Y) <- c("C1", "C2", "C3")

MDS <- as_tibble(Y) %>%
  mutate(name = rownames(ciudades_tbl))

plot_ly(MDS, x = ~C1, y = ~C2, z = ~C3) %>% 
  add_markers()

## Stress en R3 ####
dist_reconstruida <- as.matrix(dist(Y))

p_ij = dist_reconstruida[upper.tri(dist_reconstruida)]
d_ij = ciudades[upper.tri(ciudades)]
denominator = sum(p_ij^2)
nominator = sum((d_ij - p_ij)^2)
normalized_stress = sqrt(nominator/denominator)
normalized_stress

# Usando librerías ####
## Smacof ####
mds0 <- mds(delta = ciudades,ndim = 2 , type = "ordinal")
mds0
mds0$conf
plot(mds0)

## cmdscale ####
mds1 = cmdscale(ciudades, k = 2)
plot(mds1)
# plot
plot(mds1[,1], mds1[,2], type = "n", xlab = "", ylab = "", axes = FALSE,
     main = "cmdscale (stats)")
text(mds1[,1], mds1[,2], ciudades_tbl$...1, cex = 0.9, xpd = TRUE)
mds1

### Stress en R2 ####
dist_reconstruida <- as.matrix(dist(mds1))

p_ij = dist_reconstruida[upper.tri(dist_reconstruida)]
d_ij = ciudades[upper.tri(ciudades)]
denominator = sum(p_ij^2)
nominator = sum((d_ij - p_ij)^2)
normalized_stress = sqrt(nominator/denominator)
normalized_stress

### Stress en R3 ####
dist_reconstruida <- as.matrix(dist(cmdscale(ciudades, k = 3)))

p_ij = dist_reconstruida[upper.tri(dist_reconstruida)]
d_ij = ciudades[upper.tri(ciudades)]
denominator = sum(p_ij^2)
nominator = sum((d_ij - p_ij)^2)
normalized_stress = sqrt(nominator/denominator)
normalized_stress

# rotadas y reflejadas respescto a su ubicacion geografica
# stress como medida del buen ajuste.
# en este caso es bajo se perdio poco lo que era de esperar

plot(mds0)
plot(-mds0$conf[,1],-mds0$conf[,2])
text(-mds0$conf[,1], -mds0$conf[,2], names(ciudades), cex = 0.9, xpd = TRUE)

as_tibble(mds0$conf) %>% 
  bind_cols(ciudades_tbl$...1) %>%
  rename(label = `...3`) %>%
  ggplot(aes(x = -D1 , y = -D2)) + 
  geom_point(alpha = 1 , color = "blue", size = 12 ) + 
  geom_text(aes(label = label ), color = "white" ) + 
  theme_void()

# hay varias librerias
#install.packages(c("vegan", "ecodist", "labdsv", "ape", "ade4", "smacof"))
#library(stats)  
#cmdscale() 
#library(smacof)
#smacofSym() 
#library(vegan)
#wcmdscale() 
#library(ecodist)
#pco()
#library(labdsv)
#pco() 
#library(ape)
#pcoa()
#library(ade4)
#dudi.pco()


# EJEMPLO 2 ####
intensifiers <- readRDS(url("https://tinyurl.com/7k378zcd"))
# numero de ocurrencias
sum(intensifiers)

distancias <- dist(intensifiers, method="canberra", diag=T, upper=T)
head(as.matrix(distancias))
mds <- cmdscale(distancias,eig=TRUE, k=2)
mds

as_tibble(mds$points) %>% 
  bind_cols(rownames(intensifiers)) %>%
  rename(label = `...3`) %>%
  ggplot(aes(x = V1 , y = V2)) + 
  geom_text(aes(label = label ), color = "black" ) + 
  theme_void()

distancias
dist(mds$points, diag=T, upper=T)
#############extra
#El stress se calcula en funcion de las distancias originales y en  de las distancias estimadas. 
#Es decir a partir de la configuracion de los puntos obtenidos luego del escalamiento, calculamos las distancias de esos puntos esa seria  dhat_matrix  y d_matrix es la matriz de distancia de los datos originales. Luego sigo igual que en el script

dhat_matrix<- as.matrix(dist(x = mds$points, method = "euclidean"))
d_matrix = as.matrix(distancias)
p_ij = dhat_matrix[upper.tri(dhat_matrix)]
d_ij = d_matrix[upper.tri(d_matrix)]
denominator = sum(p_ij^2)
nominator = sum((p_ij - d_ij)^2)
normalized_stress = sqrt(nominator/denominator) #ver falta raiz
normalized_stress


#Y otra forma de medir el ajuste, que vimos en clase, adem?s del Stress normalizado que dice Dani, es ver el ajuste de las matrices B: 

B_orig <- -1/2 * H %*% d_matrix^2 %*% H
B_hat <- -1/2 * H %*% dhat_matrix^2 %*% H

#mediante
#norma_frob_cuadrado(B_orig - B_hat) / norma_frob_cuadrado(B orig).

#La matriz B_hat tambien se puede obtener como X %*% t(X), si X es el escalado final.