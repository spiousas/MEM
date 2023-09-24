pacman::p_load(here, tidyverse, matlib, factoextra, MASS, rgl, ggord, wordspace,
               GGally, tourr, plotly)

# Ejercicio 2.9 ####
sigma<-matrix(c(1,1/2,1/2,1),2)
pi1<-3/4
pi2<-1/4
mu1<-matrix(c(1,1/2),2,1)
mu2<-matrix(c(-1/2,1),2,1)

## a ####
mu_x<-pi1*mu1+pi2*mu2

sigmaw<-sigma
sigmab<-pi1*(mu1-mu_x)%*%t((mu1-mu_x))+pi2*(mu2-mu_x)%*%t((mu2-mu_x))
sigma.x<-sigmaw+sigmab

## b ####
eig.sigmaw<-eigen(sigma)
U<-eig.sigmaw$vectors
L<-diag(eig.sigmaw$values)

C<-U%*%sqrt(L)%*%t(U)
B<-t(solve(C))%*%sigmab%*%solve(C)
eig.B<-eigen(B)
betas<-eig.B$vectors
L.B<-diag(eig.B$values)

A<-solve(C)%*%betas

###
# comparación con la solución para el alfa1 con k=2
###
alfa1<-solve(sigmaw)%*%(mu1-mu2)
c<-t(A[,1])%*%sigma%*%A[,1]/(t(A[,1])%*%(mu1-mu2))
A[1,1]/c
A[2,1]/c
###

## c ####
v1<-t(A)%*%mu1
v2<-t(A)%*%mu2

d_euc<-t(v1-v2)%*%(v1-v2)
d_Mahalanobis<-t(mu1-mu2)%*%solve(sigma)%*%(mu1-mu2)

## d ####
sigma.z<-t(A)%*%sigma.x%*%A
sigmaw.z<-t(A)%*%sigmaw%*%A
sigmab.z<-t(A)%*%sigmab%*%A

###
#chequeo de sigmab.z
###
v_x<-pi1*v1+pi2*v2
prueba<-pi1*(v1-v_x)%*%t((v1-v_x))+pi2*(v2-v_x)%*%t((v2-v_x))
###

## e ####
n<-500
obs<-matrix(NA,n,3)
set.seed(1234)
for (i in 1:n) {
  obs[i,1]<-rbinom(1,1,0.75) #1 si es grupo 1, 0 si es grupo 2
  obs[i,2:3]<-obs[i,1]*mvrnorm(1,c(1,1/2),sigma)+(1-obs[i,1])*mvrnorm(1,c(-1/2,1),sigma)
}

colnames(obs) <- c("G", "x1", "x2")

grupo<-as.factor(obs[,1])
X<-obs[,2:3]
plot(X,col=grupo,pch=20)

## f ####
Z<-X%*%as.matrix(A)
plot(Z,col=grupo,pch=20)

## BONUS ####
### Coordenadas discriminantes ####
# Encuentra A_hat
mu1_hat <- as.matrix(colMeans(obs[obs[,1]==1,2:3]), ncol = 1)
mu2_hat <- as.matrix(colMeans(obs[obs[,1]==0,2:3]), ncol = 1)
mu_x_hat <- pi1*mu1_hat+pi2*mu2_hat

sigma_hat_1 <- cov(obs[obs[,1]==1,2:3]) 
sigma_hat_2 <- cov(obs[obs[,1]==1,2:3]) 
sigmaw_hat <- pi1*sigma_hat_1+pi2*sigma_hat_2
sigmab_hat <- pi1*(mu1_hat-mu_x_hat)%*%t((mu1_hat-mu_x_hat))+pi2*(mu2_hat-mu_x_hat)%*%t((mu2_hat-mu_x_hat))
sigma.x_hat <- sigmaw_hat+sigmab_hat
# Esta sigma.x_hat enrealidad es lo mismo que cov(obs[,2:3])

eig.sigmaw_hat <- eigen(sigmaw_hat)
U <- eig.sigmaw_hat$vectors
L <- diag(eig.sigmaw_hat$values)

C <- U%*%sqrt(L)%*%t(U)
B <- t(solve(C))%*%sigmab%*%solve(C)
eig.B <- eigen(B)
betas <- eig.B$vectors
L.B <- diag(eig.B$values)

A_CD <- solve(C)%*%betas # La matriz obtenida con coordenadas discriminantes

Z <- X%*%A_CD
plot(Z,col=grupo,pch=20)

### Correlación canónica ####
x <- obs[,2:3]
y <- obs[,1]
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

plot(U, col = as.factor(y),pch=20)

# Ejercicio 2.10 ####

## a ####
# Voy a hacer todas las sumas lo más explícitas posibles, por supuesto que se puede
# vectorizar
ss.split <- function(data, grouping) {
  U <- matrix(0, ncol(data), ncol(data))
  H <- matrix(0, ncol(data), ncol(data))
  
  for (i in 1:length(unique(grouping))) {
    group_i <- unique(grouping)[i]
    x_i <- data[grouping==group_i,]
    
    cat(paste0("El grupo ",group_i, " tiene ", nrow(x_i), " miembros\n"))
    
    for (j in 1:nrow(x_i)) {
      U <- U + as.matrix(x_i[j,] - colMeans(x_i)) %*% t(as.matrix(x_i[j,] - colMeans(x_i)))  
    }
    H <- H + nrow(x_i) * as.matrix(colMeans(x_i) - colMeans(data)) %*% t(as.matrix(colMeans(x_i) - colMeans(data)))
  }

  T_m <- U + H
  return(list(U, H, T_m))
}

## b ####
# No dice que datos así que voy a usar los del 2.9
sigma <- matrix(c(1,1/2,1/2,1),2)
mu1 <- matrix(c(1,1/2),2,1)
mu2 <- matrix(c(-1/2,1),2,1)

set.seed(123)
data <- rbind(mvrnorm(150, mu1, sigma), mvrnorm(50, mu2, sigma))
grouping <- rbind(as.matrix(rep(1, 150)), as.matrix(rep(2, 50)))

matrices <- ss.split(data, grouping)
k <- length(unique(grouping))
n <- nrow(data)
matrices[[1]]/(n-k) # Sigma within  
matrices[[2]]/(n)   # Sigma between
matrices[[3]]/(n-1) # Sigma total

# Lo hago como en el 2.9 para comparar 
pi1_hat <- sum(grouping==1)/nrow(grouping)
pi2_hat <- sum(grouping==2)/nrow(grouping)
mu1_hat <- as.matrix(colMeans(data[grouping==1,]), ncol = 1)
mu2_hat <- as.matrix(colMeans(data[grouping==2,]), ncol = 1)
mu_x_hat <- pi1_hat * mu1_hat + pi2_hat * mu2_hat

sigmaw_hat_1 <- cov(data[grouping==1,]) # Sigma within del grupo 1
sigmaw_hat_2 <- cov(data[grouping==2,]) # Sigma within del grupo 2
sigmaw_hat <- pi1_hat * sigmaw_hat_1 + pi2_hat * sigmaw_hat_2 # Sigma within total
sigmab_hat <- pi1_hat * (mu1_hat-mu_x_hat) %*% t((mu1_hat-mu_x_hat)) + pi2_hat * (mu2_hat-mu_x_hat) %*% t((mu2_hat-mu_x_hat))
sigma_hat <- sigmaw_hat + sigmab_hat
sigma_hat # DUDA Por algún motivo no dan exactamente iguales a las de ss.split

cov(data) # Sigma total con la función cov (da igual al de ss.split)

## c ####
### Poblacionales ####
sigma <- matrix(c(1,1/2,1/2,1),2)
mu1 <- matrix(c(1,1/2),2,1)
mu2 <- matrix(c(-1/2,1),2,1)
pi1 <- 3/4
pi2 <- 1/4
mu_x <- pi1*mu1+pi2*mu2
sigmaw <- sigma
sigmab <- pi1*(mu1-mu_x)%*%t((mu1-mu_x))+pi2*(mu2-mu_x)%*%t((mu2-mu_x))
sigma.x <- sigmaw+sigmab

eig.sigmaw <- eigen(sigmaw)
U <- eig.sigmaw$vectors
L <- diag(eig.sigmaw$values)

C <- U%*%sqrt(L)%*%t(U)
B <- t(solve(C))%*%sigmab%*%solve(C)
eig.B <- eigen(B)
betas <- eig.B$vectors
L.B <- diag(eig.B$values)

A <- solve(C)%*%betas
A

# Nuevas medias en el espacio transformado
v1 <- t(A) %*% mu1
v1
v2 <- t(A) %*% mu2
v2

### Muestrales ####
mu1_hat <- as.matrix(colMeans(data[grouping==1,]), ncol = 1)
mu2_hat <- as.matrix(colMeans(data[grouping==2,]), ncol = 1)
matrices <- ss.split(data, grouping)
k <- length(unique(grouping))
n <- nrow(data)
sigmaw_hat <- matrices[[1]]/(n-k) # Sigma within  
sigmaw_hat
sigmab_hat <- matrices[[2]]/(n)   # Sigma between
sigmab_hat

eig.sigmaw <- eigen(sigmaw_hat)
U <- eig.sigmaw$vectors
L <- diag(eig.sigmaw$values)

C <- U%*%sqrt(L)%*%t(U)
B <- t(solve(C))%*%sigmab_hat%*%solve(C)
eig.B <- eigen(B)
betas <- eig.B$vectors
L.B <- diag(eig.B$values)

A_hat <- solve(C)%*%betas

# Nuevas medias en el espacio transformado
v1_hat <- t(A_hat) %*% mu1_hat
v1
v1_hat
v2_hat <- t(A_hat) %*% mu2_hat
v2
v2_hat

# Son estimaciones bastante razonables

# Ejercicio 2.11 ####

## Mi función para CD
CD_Spiousas <- function(data, grouping, center = T, scale = F) {
  data <- scale(data, scale = scale, center = center)
  
  matrices <- ss.split(data, grouping)

  n <- nrow(data)
  k <- length(unique(grouping))

  sigmaw_hat <- matrices[[1]]/(n-k) # Sigma within  
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
  
  prop_trace <- eig.B$values[1:k-1]/sum(eig.B$values)
  salida <- list(prop_trace, A_hat[,1:k-1], eig.B$values[1:k-1])
  names(salida) <- c("prop_trace", "rotation", "eigenvalues")
  salida
}

## a ####
data_cocos <- read_csv(here("visualización/TPs/TP2_CD/data/cocodrilos.csv"))
X_cocos <- scale(data_cocos[,1:11])
especies <- data_cocos[,12]

matrices <- ss.split(X_cocos, especies)
n <- nrow(X_cocos)
k <- nrow(unique(especies))
sigmaw_hat <- matrices[[1]]/(n-k) # Sigma within  
sigmab_hat <- matrices[[2]]/(n) # Sigma between
sigma_hat <- matrices[[3]]/(n-1) # Sigma total
sigma_hat - cov(X_cocos) # Dan iguales ambas matrices

eig.sigmaw <- eigen(sigmaw_hat)
U <- eig.sigmaw$vectors
L <- diag(eig.sigmaw$values)

C <- U%*%sqrt(L)%*%t(U)
B <- t(solve(C))%*%sigmab_hat%*%solve(C)
eig.B <- eigen(B)
betas <- eig.B$vectors
L.B <- diag(eig.B$values)

A_hat <- solve(C)%*%betas
A_hat # <- Las transformaciones

# Los datos transformados
X_cocos_transf <- X_cocos %*% A_hat[,1:3]
colnames(X_cocos_transf) <- c("cd1", "cd2", "cd3")

## b ####
# Este plot3d es de la librería rgl
plot3d(X_cocos_transf, 
       col = especies$especie, 
       cube=TRUE, 
       size = 10)

figura <- plot_ly(data.frame(X_cocos_transf),x = ~cd1, y = ~cd2, z = ~cd3, color = ~as.factor(especies$especie))
figura <- figura %>% add_markers()
figura <- figura %>% layout(scene = list(xaxis = list(title = 'C1'),
                                         yaxis = list(title = 'C2'),
                                         zaxis = list(title = 'C3')))

figura

# Al igual que en PCA las varianzas explicadas son los autovalores de B dividido su suma
eig.B$values[1:3]/sum(eig.B$values)

# A partir del 4to autovalor valen cero, es decir, pareciera que sólo se peuden tener 
# tres k-1 coordenadas discriminantes que expliquen varianza (siendo k el número de grupos)

## c ####
# Calculo el PCA
A_pca <- eigen(sigma_hat)$vectors

# Los datos transformados con PCA
X_cocos_transf_PCA <- X_cocos %*% A_pca[,1:3]

# Este plot3d es de la librería rgl
plot3d(X_cocos_transf_PCA, 
       col = especies$especie, 
       cube=TRUE, 
       size = 10)

figura

eigen(sigma_hat)$values[1:3]/sum(eigen(sigma_hat)$values)

# Si bien la primera componente de PCA explica mucha varianza, no optimiza la 
# separación entre grupos.

## Repito b y c usando funciones ####
# Primero coordenadas discriminantes
especies <- data_cocos[,12]$especie
X <- data_cocos[,1:11]
# Sin centrar
CD_cocos <- lda(especies~., data = X)
CD_cocos
CD_Spiousas(data = X, grouping = especies, center = F, scale = F)

# Centrado
CD_cocos <- lda(especies~., data = as.data.frame(X_cocos))
CD_cocos
CD_Spiousas(data = X, grouping = especies, center = T, scale = T)

eig.B$values[1:3]/sum(eig.B$values)
A_hat[,1:3]
# DUDA Son como versiones escaleadas, pero da lo mismo

# trasnformo los datos y ploteo en 3d
X_cocos_transf <- X_cocos %*% CD_cocos$scaling
colnames(X_cocos_transf) <- c("cd1", "cd2", "cd3")

## b ####
# Este plot3d es de la librería rgl
plot3d(X_cocos_transf, 
       col = especies, 
       cube=TRUE, 
       size = 10)
# Hay unos cambios de signo pero la magnitud es la misma

# Ejercicio 2.12 ####
data_olmos <- read_csv(here("visualización/TPs/TP2_CD/data/olmos.csv"))
X_olmos <- data_olmos %>% dplyr::select(-cat)
category <- data_olmos$cat

## Sin estandarizar ####
CD_olmos <- lda(category~., data = X_olmos)
CD_olmos

CD_olmos_mio <- CD_Spiousas(data = X_olmos, grouping = category, center = F)
ggord(CD_olmos, category)
# Sin estandarizar las cd están en un plano que contiene a X1 y es perpendicular a
# las demás DUDA No termino de entender bien por qué

## Estandarizando ####
CD_olmos_std <- lda(category~., data = as_tibble(scale(X_olmos)))
CD_olmos_std

ggord(CD_olmos_std, category)

set.seed(123)
data <- rbind(mvrnorm(150, mu1, sigma), mvrnorm(50, mu2, sigma), mvrnorm(50, mu1+mu2, sigma))
grouping <- rbind(as.matrix(rep(1, 150)), as.matrix(rep(2, 50)), as.matrix(rep(3, 50)))
lda(grouping~., data = as.data.frame(data))
ggord(lda(grouping~., data = as.data.frame(data)))
# Cuando estandarizamos se reparte mejor la proyección de las variables en el biplot.
# Pero la varianza explicada por cada componente es la misma y pareciera que las 
# coordenadas discriminantes son las mismas.

# DUDA: No sé de que supuesto de igualdad de varianzas me habla

# Ejercicio 2.13 ####
clasif.fisher <- function(x, grouping, y) {
  CD <- lda(grouping~., data = as_tibble(x))
  n_groups <- length(CD$counts)
  
  # Calculo las coordenadas y los centras en las CD
  new_data_transf <- y %*% CD$scaling
  centers <- CD$means %*% CD$scaling
  
  # Inicializo vector de distancias
  distances <- matrix(0, nrow(new_data_transf), n_groups)
  
  # Para cada grupo calculo la distancia con respecto al centro en las CD
  for (i in 1:n_groups) {
    distances[,i] <- rowNorms(sweep(new_data_transf, 2, centers[i,]), method = "euclidean")
  }
  
  # Me fijo a qué centro están mas cerca
  group_pred <- c()
  for (j in 1:nrow(distances)) {
    group_pred[j] = which.min(distances[j,]) 
  }
  
  group_pred
}

sum(clasif.fisher(X_cocos, especies, X_cocos) == especies)/44

# Qué pasa si lo hacía sin transformar
clasif.vanilla <- function(x, grouping, y) {
  n_groups <- length(unique(grouping))
  
  centers <- matrix(0, n_groups, ncol(x))
  # Calculo los centros de cada grupo
  for (i in 1:n_groups) {
    centers[i,] <- colMeans(x[grouping==i,])
  }
  
  # Inicializo vector de distancias
  distances <- matrix(0, nrow(y), n_groups)

  # Para cada grupo calculo la distancia con respecto al centro en las CD
  for (i in 1:n_groups) {
    distances[,i] <- rowNorms(sweep(y, 2, centers[i,]), method = "euclidean")
  }

  # Me fijo a qué centro están mas cerca
  group_pred <- c()
  for (j in 1:nrow(distances)) {
    group_pred[j] = which.min(distances[j,])
  }

  group_pred
}

sum(clasif.vanilla(X_cocos, especies, X_cocos)== especies)/44

# Clasifica mucho mejor en el espacio de las coordenadas discriminantes que en el espacio
# original