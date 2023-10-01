pacman::p_load(here, tidyverse, MASS, plotly, factoextra, fpc, janitor, 
               StatMatch, factoextra, smacof)

# Ejercicio 1 ####
## a ####
Sigma <- matrix(c(1,-1,-1,9), nrow = 2)
Sigma

rho <- matrix(c(1,-1/3,-1/3,1), nrow = 2)
rho
A <- eigen(rho)$vectors
A

# Simulo para ver que esté dando más o menos bien
X <- mvrnorm(mu = c(0,0), Sigma = Sigma, n = 1000000)
prcomp(X)
cov(scale(X) %*% eigen(cor(X))$vectors)

## b ####
b1 <- matrix(A[,1])
b1

t(b1) %*% matrix(c(-1,1), nrow = 2)
# Da sqrt(2)

matrix(c(-1,1), nrow = 1) %*% eigen(cor(X))$vectors

# Ejercicio 2 ####
data_camiones <- read_csv(here("visualización/parcial/data/camiones.csv")) %>%
  rename(CostoM = X1,
         CostoR = X2,
         CostoC = X3)
data_camiones$Tipo

ss.split <- function(data, grouping) {
  U <- matrix(0, ncol(data), ncol(data))
  H <- matrix(0, ncol(data), ncol(data))
  
  for (i in 1:length(unique(grouping))) {
    group_i <- unique(grouping)[i]
    x_i <- data[grouping==group_i,]
    print(x_i)
    cat(paste0("El grupo ",group_i, " tiene ", length(x_i), " miembros\n"))
    
    for (j in 1:length(x_i)) {
      U <- U + as.matrix(x_i[j,] - colMeans(x_i)) %*% t(as.matrix(x_i[j,] - colMeans(x_i)))  
    }
    H <- H + length(x_i) * as.matrix(colMeans(x_i) - colMeans(data)) %*% t(as.matrix(colMeans(x_i) - colMeans(data)))
  }
  
  T_m <- U + H
  return(list(U, H, T_m))
}

CD_Spiousas <- function(data, grouping, center = T, scale = F) {
  data <- scale(data, scale = scale, center = center)
  
  matrices <- ss.split(data, grouping)
  
  n <- nrow(data)
  k <- length(unique(grouping))
  
  sigmaw_hat <- matrices[[1]]/(n-1) # Sigma within  
  sigmab_hat <- matrices[[2]]/(n-1) # Sigma between
  sigma_hat <- matrices[[3]]/(n-1) # Sigma total
  
  eig.sigmaw <- eigen(as.matrix(sigmaw_hat))
  U <- eig.sigmaw$vectors
  L <- diag(eig.sigmaw$values)
  
  C <- U%*%sqrt(L)%*%t(U)
  B <- t(solve(C))%*%sigmab_hat%*%solve(C)
  eig.B <- eigen(B)
  betas <- eig.B$vectors
  L.B <- diag(eig.B$values)
  
  A_hat <- solve(C)%*%betas
  
  prop_trace <- eig.B$values/sum(eig.B$values)
  salida <- list(data, prop_trace, A_hat, eig.B$values)
  names(salida) <- c("x", "prop_trace", "rotation", "eigenvalues")
  salida
}

## a ####
grupo = if_else(data_camiones$Tipo == "diesel", 0, 1)

CD <- CD_Spiousas(data_camiones[,-4], grupo)
CD

U <- CD$x %*% CD$rotation

matrices <- ss.split(U, grouping = grupo)

n <- nrow(U)

sigmaw_hat <- matrices[[1]]/(n-1) # Sigma within  
round(sigmaw_hat, 3)
sigmab_hat <- matrices[[2]]/(n-1) # Sigma between
round(sigmab_hat, 3)

tibble(X = CD$x %*% CD$rotation,
       Tipo = data_camiones$Tipo) %>%
  ggplot(aes(x = X[,1],
             fill = Tipo,
             color = Tipo)) +
  #facet_grid(grupo~.) +
  geom_density(alpha = .2) +
  theme_bw() +
  labs(x = "CD1") +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "top")

LDA <- lda(Tipo~., data = data_camiones)    

clas <- cbind(predict(LDA, newdata = data_camiones)$x ,predict(LDA, newdata = data_camiones)$class)
clas[clas[,2]==2,]

## b ####
beta1 <- CD$rotation[,1]

tibble(X = CD$x %*% beta1,
       Tipo = data_camiones$Tipo) %>%
  ggplot(aes(x = X[,1],
             fill = Tipo,
             color = Tipo)) +
  #facet_grid(grupo~.) +
  geom_density(alpha = .2) +
  geom_vline(xintercept = t(beta1) %*% (meandie + meannaft)/2 ) +
  theme_bw() +
  labs(x = "CD1") +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "top")

## c ####
Xdie <- scale(data_camiones[data_camiones$Tipo == "diesel",-4], scale = F)
Xnaft <- scale(data_camiones[data_camiones$Tipo == "diesel",-4], scale = F)
colMeans(Xdie)

## d ####
x <- as.matrix(data_camiones[,1:3])
y <- as.matrix(if_else(data_camiones[,4] == "naftero", 1, 0))
sigma_x <- cov(x)
sigma_y <- var(y)
sigma_xy <- cov(x,y)

# Voy a llamar Ups a la matriz Upsilon
Ups <- solve(sigma_x) %*% sigma_xy %*% solve(sigma_y) %*% t(sigma_xy)
Ups

# Ahora calculo los autovectores de esto
eig <- eigen(Ups)
m_1 <- matrix(eig$vectors[,1])
m_1
m_2 <- matrix(eig$vectors[,2])
m_2

# alpha_1 va a ser m_1 escalado a que la varianza de U1 sea 1
# Lo divido por la sd entonces
k_1 <- 1/sqrt(t(m_1) %*% sigma_x %*% m_1)
k_2 <- 1/sqrt(t(m_2) %*% sigma_x %*% m_2)
alpha_1_CC <- as.numeric(k_1)*m_1
alpha_2_CC <- as.numeric(k_2)*m_2
alpha_1_CC
alpha_2_CC
A_CC <- cbind(alpha_1_CC, alpha_2_CC)
U <- x %*% A_CC

tibble(X = x %*% A_CC,
       Tipo = data_camiones$Tipo) %>%
  ggplot(aes(x = X[,1],
             fill = Tipo,
             color = Tipo)) +
  #facet_grid(grupo~.) +
  geom_density(alpha = .2) +
  theme_bw() +
  labs(x = "CC1") +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "top")

matrices <- ss.split(as.matrix(U), grouping = grupo)

n <- nrow(U)

sigmaw_hat <- matrices[[1]]/(n-1) # Sigma within  
round(sigmaw_hat, 3)
sigmab_hat <- matrices[[2]]/(n-1) # Sigma between
round(sigmab_hat, 3)

# Ejercicio 3 ####
data_hortalizas <- read.table(here("visualización/parcial/data/hortalizas.txt"))
X <- as.matrix(data_hortalizas)

## a ####
### Calculo el MDS ####
A <- -0.5*X^2
n <- dim(X)[1]
H <- diag(rep(1,n))-n^{-1}*(rep(1,n))%*%t(rep(1,n))
B <- H %*% A %*% H
DEL <- eigen(B)$values
round(eigen(B)$values,3)
V <- eigen(B)$vectors

round(B-V%*%diag(DEL)%*%t(V))

### Proyecto en R2 ####
V2<-V[,1:2]
t(V2)%*%(V2)
DEL2<-diag(DEL[1:2])
Y2<-V2%*%sqrt(DEL2)
colnames(Y2) <- c("C1", "C2")

MDS <- as_tibble(Y2) %>%
  mutate(name = colnames(data_hortalizas))

MDS %>% ggplot(aes(x = C1,
                   y = C2)) +
  geom_text(aes(label = name)) +
  theme_bw() +
  labs(color = "Grupo:") +
  theme(legend.position = "top")

### El Strain para R2 ####
norm(B-Y2%*%t(Y2), type = "f")^2/norm(B, type = "f")^2

norm(B-Y2%*%t(Y2), type = "f")^2
sum(eigen(B)$values[-c(1:2)]^2)

### Proyecto en R3 ####
V3 <- V[,1:3]
t(V3) %*% (V3)
DEL3 <- diag(DEL[1:3])
Y3 <- V3%*%sqrt(DEL3)
colnames(Y3) <- c("C1", "C2", "C3")

MDS <- as_tibble(Y3) %>%
  mutate(name = colnames(data_hortalizas))

plot_ly(MDS, x = ~C1, y = ~C2, z = ~C3) %>% 
  add_markers()

### El Strain para R3 ####
norm(B-Y3%*%t(Y3), type = "f")^2/norm(B, type = "f")^2

norm(B-Y3%*%t(Y3), type = "f")^2
sum(eigen(B)$values[-c(1:3)]^2)

### Todas las dimensiones con autov no nulos ####
V5<-V[,1:5]
t(V5)%*%(V5)
DEL5 <- diag(DEL[1:5])
Y5 <- V5%*%sqrt(DEL5)
colnames(Y5) <- c("C1", "C2", "C3", "C4", "C5")

### El Strain para R5 ####
norm(B-Y5%*%t(Y5), type = "f")^2/norm(B, type = "f")^2

norm(B-Y5%*%t(Y5), type = "f")^2
sum(eigen(B)$values[-c(1:5)]^2)

## b ####
distancia <- as.dist(data_hortalizas)

## b ####
hc <- hclust(distancia, method = "centroid")
plot(hc)
# Me parecen razonables 7 grupos
rect.hclust(hc, k=4, border="red")
hc$labels
cutree(hc, k=4) 

### Ahora lo hago con las distancias del escalado
distMDS <- dist(MDS[,-3])
names(distMDS) <- names(data_hortalizas)

hc_mds <- hclust(distMDS, method = "centroid")
plot(hc_mds)
# Me parecen razonables 7 grupos
rect.hclust(hc_mds, k=4, border="red")
hc_mds$labels
cutree(hc_mds, k=4) 

## c ####
strain <- rep(NA, 6)
for (i in 2:5 ) {
  print(i)
  V_i <- V[,1:i]
  DEL_i <- diag(DEL[1:i])
  Y_i <- V_i%*%sqrt(DEL_i)
  strain[i] <- norm(B-Y_i%*%t(Y_i), type = "f")^2/norm(B, type = "f")^2
  norm(B-Y_i%*%t(Y_i), type = "f")^2
  sum(eigen(B)$values[-c(1:i)]^2)
}
strain
