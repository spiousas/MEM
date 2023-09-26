rm(list=ls())
library(MASS)

sigma<-matrix(c(1,1/2,1/2,1),2)
pi1<-3/4
pi2<-1/4
mu1<-matrix(c(1,1/2),2,1)
mu2<-matrix(c(-1/2,1),2,1)

# item a ####
mu_x<-pi1*mu1+pi2*mu2

sigmaw<-sigma
sigmab<-pi1*(mu1-mu_x)%*%t((mu1-mu_x))+pi2*(mu2-mu_x)%*%t((mu2-mu_x))
sigma.x<-sigmaw+sigmab

# item b ####
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

#item c ####
v1<-t(A)%*%mu1
v2<-t(A)%*%mu2

d_euc<-t(v1-v2)%*%(v1-v2)
d_Mahalanobis<-t(mu1-mu2)%*%solve(sigma)%*%(mu1-mu2)

#item d ####
sigma.z<-t(A)%*%sigma.x%*%A
sigmaw.z<-t(A)%*%sigmaw%*%A
sigmab.z<-t(A)%*%sigmab%*%A

###
#chequeo de sigmab.z
###
v_x<-pi1*v1+pi2*v2
prueba<-pi1*(v1-v_x)%*%t((v1-v_x))+pi2*(v2-v_x)%*%t((v2-v_x))
###

#item e ####
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

#item f
Z<-X%*%as.matrix(A)
plot(Z,col=grupo,pch=20)

#BONUS ####
##Coordenadas discriminantes ####
# Encuentra A_hat
mu1_hat <- as.matrix(colMeans(obs[obs[,1]==1,2:3]), ncol = 1)
mu2_hat <- as.matrix(colMeans(obs[obs[,1]==0,2:3]), ncol = 1)
mu_x_hat <- pi1*mu1_hat+pi2*mu2_hat

sigma_hat_1 <- cov(obs[obs[,1]==1,2:3]) 
sigma_hat_2 <- cov(obs[obs[,1]==2,2:3]) 
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

##Correlación canónica ####
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
