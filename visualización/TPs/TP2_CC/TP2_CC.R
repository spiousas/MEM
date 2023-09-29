pacman::p_load(here, tidyverse, matlib, factoextra, MASS, vegan, CCA, reshape)

# Ejercicio 2.7 ####
## a ####
sigma_x <- matrix(c(8,2,2,5), nrow = 2)
sigma_x
sigma_y <- matrix(c(6,-2,-2,7), nrow = 2)
sigma_y
sigma_xy <- matrix(c(3,1,-1,3), nrow = 2)
sigma_xy

# Voy a llamar Ups a la matriz Upsilon
Ups <- inv(sigma_x) %*% sigma_xy %*% inv(sigma_y) %*% t(sigma_xy)
Ups

# Ahora calculo los autovectores de esto
eig <- eigen(Ups)
m_1 <- matrix(eig$vectors[,1])
m_1

# alpha_1 va a ser m_1 escalado a que la varianza de U1 sea 1
# Lo divido por la sd entonces
k <- 1/sqrt(t(m_1) %*% sigma_x %*% m_1)
alpha_1 <- as.numeric(k)*m_1
alpha_1

# Veo la notación de Izenman para chequear que sea equivalente
# Factorizo para obtener sigma_y^(-1/2)
E <- eigen(inv(sigma_y))
V <- E$vectors; U <- solve(V)
D <- diag(E$values)
sigma_y_inv_sqrt <- V %*% D^(1/2) %*% U

R <- sigma_y_inv_sqrt %*% t(sigma_xy) %*% inv(sigma_x) %*% sigma_xy %*% sigma_y_inv_sqrt
eig_R <- eigen(R)
r_1 <- matrix(eig_R$vectors[,1])
m_1_Izenman <- inv(sigma_x) %*% sigma_xy %*% sigma_y_inv_sqrt %*% r_1
m_1_Izenman  

k <- 1/sqrt(t(m_1_Izenman) %*% sigma_x %*% m_1_Izenman)
alpha_1_Izenman <- as.numeric(k)*m_1_Izenman
alpha_1_Izenman
# Da invertido el signo pero es equivalente

# Ahora vamos a calcular Var(U1)
sigma_U1 <- t(alpha_1) %*% sigma_x %*% alpha_1
sigma_U1 # La varianza da 1

# Hago algo parecido con beta_1
n_1 <- 1/eig$values[1] * inv(sigma_y) %*% t(sigma_xy) %*% alpha_1
k <- 1/sqrt(t(n_1) %*% sigma_y %*% n_1)
beta_1 <- as.numeric(k)*n_1
beta_1

# Forma alternativa con Upsilon2
Ups_2 <- solve(sigma_y) %*% t(sigma_xy) %*% solve(sigma_x) %*% sigma_xy
eig_2 <- eigen(Ups_2)
n_1_alt <- matrix(eig_2$vectors[,1])
k <- 1/sqrt(t(n_1_alt) %*% sigma_y %*% n_1_alt)
beta_1_alt <- as.numeric(k)*n_1_alt
beta_1_alt

# Loa autovalores de anbas Upsilons son iguales
eig$values
eig_2$values

# Acá también chequeo lo de Izenman
n_1_Izenman <- sigma_y_inv_sqrt %*% r_1
k <- 1/sqrt(t(n_1_Izenman) %*% sigma_y %*% n_1_Izenman)
beta_1_Izenman <- as.numeric(k)*n_1_Izenman
beta_1_Izenman
# Es consistente con lo otro y me da cambiado el signo

# Ahoca calculo Var(V1)
sigma_V1 <- t(beta_1) %*% sigma_y %*% beta_1
sigma_V1

# Y su covarianza, cov(U1, V1)
sigma_U1V1 <- t(alpha_1) %*% sigma_xy %*% beta_1
sigma_U1V1

# Y esta covarianza (que en realidad es la correlación) es la raiz cuadrada del autovalor
sqrt(eig$values[1])

## b ####
# Repetimos todo pero con el segundo autovector
m_2 <- matrix(eig$vectors[,2])
m_2

# alpha_1 va a ser m_1 escalado a que la varianza de U1 sea 1
# Lo divido por la sd entonces
k <- 1/sqrt(t(m_2) %*% sigma_x %*% m_2)
alpha_2 <- as.numeric(k)*m_2
alpha_2

# Ahora vamos a calcular Var(U1)
sigma_U2 <- t(alpha_2) %*% sigma_x %*% alpha_2
sigma_U2

# Hago algo parecido con beta_1
n_2 <- 1/eig$values[1] * inv(sigma_y) %*% t(sigma_xy) %*% alpha_2
k <- 1/sqrt(t(n_2) %*% sigma_y %*% n_2)
beta_2 <- as.numeric(k)*n_2
beta_1

# Ahoca calculo Var(V1)
sigma_V2 <- t(beta_1) %*% sigma_y %*% beta_1
sigma_V2

# Y su covarianza, cov(U1, V1)
sigma_U2V2 <- t(alpha_2) %*% sigma_xy %*% beta_2
sigma_U2V2

# Y esta covarianza (que en realidad es la correlación) es la raiz cuadrada del autovalor
sqrt(eig$values[2])

## c ####
sigma_U1U2 <- t(alpha_1) %*% sigma_x %*% alpha_2
sigma_U1U2

sigma_U1V2 <- t(alpha_1) %*% sigma_xy %*% beta_2
sigma_U1V2

sigma_V1U2 <- t(beta_1) %*% t(sigma_xy) %*% alpha_2
sigma_V1U2

sigma_V1V2 <- t(beta_1) %*% t(sigma_xy) %*% beta_2
sigma_V1V2

# Matricialmente
G <- cbind(alpha_1, alpha_2)
G

H <- cbind(beta_1, beta_2)
H

sigma_U <- t(G) %*% sigma_x %*% G
sigma_U

sigma_V <- t(H) %*% sigma_y %*% H
sigma_V

sigma_UV <- t(G) %*% sigma_xy %*% H
round(sigma_UV, digits = 2)

sigma_W <- round(rbind(cbind(sigma_U, sigma_UV), cbind(sigma_UV, sigma_V)), 2)
colnames(sigma_W) <- c("U1", "U2", "V1", "V2")
rownames(sigma_W) <- c("U1", "U2", "V1", "V2")
heatmap(sigma_W, Rowv = NA, Colv = NA)

## e ####
# Armo los versores
e1 <- matrix(c(1,0), nrow = 2)
e2 <- matrix(c(0,1), nrow = 2)

sigma_Ux <- t(G) %*% sigma_x
sigma_Ux

sigma_Uy <- t(G) %*% sigma_xy
sigma_Uy

sigma_Vx <- t(H) %*% t(sigma_xy)
sigma_Vx

sigma_Vy <- t(H) %*% sigma_y
sigma_Vy

# Vuelvo a calcular estandarizando 
sigma_U <- t(G) %*% sigma_x %*% G
sigma_U

sigma_V <- t(H) %*% sigma_y %*% H
sigma_V

sigma_UV <- t(G) %*% sigma_xy %*% H
round(sigma_UV, digits = 2)


# Armo las matrices grandes
sigma_z <- rbind(cbind(sigma_x, sigma_xy), cbind(t(sigma_xy), sigma_y))
colnames(sigma_z) <- c("x1", "x2", "y1", "y2")
rownames(sigma_z) <- c("x1", "x2", "y1", "y2")
round(sigma_z, digits = 3)

sigma_w <- rbind(cbind(sigma_U, sigma_UV), cbind(t(sigma_UV), sigma_V))
colnames(sigma_w) <- c("u1", "u2", "v1", "v2")
rownames(sigma_w) <- c("u1", "u2", "v1", "v2")
round(sigma_w, digits = 3)

sigma_wz <- rbind(cbind(sigma_Ux, sigma_Uy), cbind(sigma_Vx, sigma_Vy))
colnames(sigma_wz) <- c("u1", "u2", "v1", "v2")
rownames(sigma_wz) <- c("x1", "x2", "y1", "y2")
round(sigma_wz, digits = 3)

sigma_total <- rbind(cbind(sigma_z, sigma_wz), cbind(t(sigma_wz), sigma_w))
round(sigma_total, digits = 3)

heatmap(sigma_total, Rowv = NA, Colv = NA)

## f ####
set.seed(1234)
X <- mvrnorm(n = 5000, mu = c(0,0,0,0), Sigma = sigma_z)
colnames(X) <- c("x1", "x2", "y1", "y2")
head(X)

round(colMeans(X), digits = 2)
round(cov(X), digits = 2)
sigma_z

U <- X[,1:2] %*% G
round(cov(U), digits = 2)
V <- X[,3:4] %*% H
round(cov(V), digits = 2)

W <- cbind(U,V)
colnames(W) <- c("u1", "u2", "v1", "v2")
head(W)

round(colMeans(W), digits = 2)
round(cov(W), digits = 2)

ZW <- cbind(X,W)
round(colMeans(ZW), digits = 2)
round(cov(ZW), digits = 2)
round(sigma_total, digits = 2)

## g ####
U <- scale(X[,1:2]) %*% G
V <- scale(X[,3:4]) %*% H

pairs(as_tibble(X))

W <- cbind(U, V)
colnames(W) <- c("u1", "u2", "v1", "v2")
pairs(as_tibble(W))
cor(W)
sqrt(eig$values[1])
sqrt(eig$values[2])
CCorA(X[,3:4], X[,1:2])

# Ejercicio 2.8 ####
data_autos <- read_delim(here("visualización/TPs/TP2_CC/data/autos.txt"))
data_autos

X <- as.matrix(data_autos %>% dplyr::select(all_of(c("X1", "X2", "X5", "X7", "X8"))))
X

Y <- as.matrix(data_autos %>% dplyr::select(all_of(c("X3", "X4"))))
Y 

# Calculos las varianzas
sigma_x <- cov(X)
sigma_x
sigma_y <- cov(Y)
sigma_y
sigma_xy <- cov(X, Y)
sigma_xy

# Voy a llamar Ups a la matriz Upsilon
Ups <- solve(sigma_x) %*% sigma_xy %*% solve(sigma_y) %*% t(sigma_xy)
Ups

# Ahora calculo los autovectores de esto
eig <- eigen(Ups)
eig

# alpha_1
m_1 <- matrix(eig$vectors[,1])
k <- 1/sqrt(t(m_1) %*% sigma_x %*% m_1)
alpha_1 <- as.numeric(k)*m_1
alpha_1

# beta_1
n_1 <- 1/eig$values[1] * inv(sigma_y) %*% t(sigma_xy) %*% alpha_1
k <- 1/sqrt(t(n_1) %*% sigma_y %*% n_1)
beta_1 <- as.numeric(k)*n_1
beta_1

# alpha_2
m_2 <- matrix(eig$vectors[,2])
k <- 1/sqrt(t(m_2) %*% sigma_x %*% m_2)
alpha_2 <- as.numeric(k)*m_2
alpha_2

# beta_2
n_2 <- 1/eig$values[2] * inv(sigma_y) %*% t(sigma_xy) %*% alpha_2
k <- 1/sqrt(t(n_2) %*% sigma_y %*% n_2)
beta_2 <- as.numeric(k)*n_2
beta_2

u1 <- X %*% alpha_1
v1 <- Y %*% beta_1

u2 <- X %*% alpha_2
v2 <- Y %*% beta_2

W <- cbind(u1, u2, v1, v2)
colnames(W) <- c("u1", "u2", "v1", "v2")

# Dan lo mismo
round(cov(W), 3)
round(cor(W), 3)

# Usamos la funcion cancor
ccaXY <- cancor(X, Y)
ccaXY
eig$values[1]^(1/2)
eig$values[2]^(1/2)

# Función de CCA (la dejo acá)
ccaXY <- cc(X, Y)

# Las correlaciones tienen que dar eso
cca <- CCorA(Y, X)
cca$CanCorr

eig$values[1]^(1/2)
eig$values[2]^(1/2)

pairs(W)

biplot(cca)

t(as.matrix(ccaXY$xcoef))%*%sigma_x%*%as.matrix(ccaXY$xcoef) # Las varianzas no me dan 1, normalizo
XautovecST <- as.matrix(ccaXY$xcoef) * 1/sqrt(diag(t(as.matrix(ccaXY$xcoef))%*%sigma_x%*%as.matrix(ccaXY$xcoef)))
t(XautovecST)%*%sigma_x%*%XautovecST
# Por eso las columnas de XautovecST me dan igual a alpha1 y alpha2

# Lo mismo para Y
YautovecST <- as.matrix(ccaXY$ycoef) * 1/sqrt(diag(t(as.matrix(ccaXY$ycoef))%*%sigma_y%*%as.matrix(ccaXY$ycoef)))

## a, b, c y d ####
# Usamos la funcion CCorA de {vegan}
cca <- CCorA(Y,X)
cca
biplot(cca)

# Hay una manera de pedirle ploetar ambos grupos contra U y ambos grupos contra V
par(mfrow=c(1,1))

biplot(x = cca$Cx, y = rbind(cca$corr.X.Cx), 
       xlabs = rep("·", nrow(cca$Cx)), 
       main="U1 y U2 vs X")

biplot(x = cca$Cy, y = rbind(cca$corr.Y.Cy), 
       xlabs = rep("·", nrow(cca$Cy)), 
       main="U1 y U2 vs X e Y")

biplot_Spiousas(X = scale(X), V = cbind(alpha_1, alpha_2), scale = 4)
biplot_Spiousas(X = scale(Y), V = cbind(beta_1, beta_2), scale = 5)

biplot(x =cca$Cx,y =rbind(cca$corr.X.Cx, cca$corr.Y.Cy), xlabs = rep("x", nrow(cca$Cx)), main="U1 y U2 vs X e Y")
biplot(x =cca$Cy,y =rbind(cca$corr.X.Cx, cca$corr.Y.Cy), xlabs = rep("x", nrow(cca$Cx)), main="V1 y V2 vs X e Y")
biplot(x = cbind(cca$Cx[,1], cca$Cy[,1]), y = rbind(cbind(cca$corr.X.Cx[,1],cca$corr.X.Cy[,1])
                                                  , cbind(cca$corr.Y.Cx[,1],cca$corr.Y.Cy[,1])), 
       xlabs = rep("x", nrow(cbind(cca$Cx[,1], cca$Cy[,1]))),
       main="U1 y V1 vs X e Y")
biplot(x =cbind(cca$Cx[,2], cca$Cy[,2]),y = rbind(cbind(cca$corr.X.Cx[,2],cca$corr.X.Cy[,2])
                                                  , cbind(cca$corr.Y.Cx[,2],cca$corr.Y.Cy[,2])),
       xlabs = rep("x", nrow(cbind(cca$Cx[,2], cca$Cy[,2]))),
       main="U2 y V2 vs X e Y")

