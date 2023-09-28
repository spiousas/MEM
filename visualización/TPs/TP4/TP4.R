pacman::p_load(here, tidyverse, MASS, plotly, GGally)

# Ejercicio 4.2 ####
sigma <- matrix(c(1,1/2,1/2,1),2)
pi1 <- 3/4
pi2 <- 1/4
mu1 <- matrix(c(1,1/2),2,1)
mu2 <- matrix(c(-1/2,1),2,1)

## b ####
g_hat_1 <- function(x) {
  if_else((x[2] - x[1]) < 0, 1, 2)
}

g_hat_2 <- function(x) {
  if_else(x[2] - exp(x[1]-5/8) + 3/8 < 0, 1, 2)
}

## c ####
nRep <- 1000
X <- matrix(NA, nRep, 2)
Y <- matrix(NA, nRep, 1)

set.seed(123)
for (i in 1:nRep) {
  Y[i] <- rbinom(1, 1, pi1) #1 si es grupo 1, 0 si es grupo 2
  X[i,] <- Y[i] * mvrnorm(1, mu1, sigma) + (1-Y[i]) * mvrnorm(1, mu2, sigma)
}
Y <- if_else(Y==0, 2, 1)

Y_hat1 <- matrix(NA, nRep, 1)
Y_hat2 <- matrix(NA, nRep, 1)

for (i in 1:nRep) {
  Y_hat1[i] <- g_hat_1(X[i,])
  Y_hat2[i] <- g_hat_2(X[i,])
}
1 - sum(Y_hat1==Y)/nRep # El teórico es 0.248
1 - sum(Y_hat2==Y)/nRep

## d ####
colnames(X) <- c("X1", "X2")

data_sim <- as_tibble(X) %>% 
  cbind(as_tibble(Y)) 

data_sim %>% ggplot(aes(x = X1,
           y = X2,
           color = factor(Y))) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "top")
  
# para el clasificador 1 la frontera es X1=X2
data_sim %>% ggplot(aes(x = X1,
                        y = X2,
                        color = factor(Y))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, size = 1) +
  theme_bw() +
  labs(color = "Grupo") +
  theme(legend.position = "top")

# para el clasificador 1 la frontera es X2=exp(X1-5/8)-3/8
frontera2 <- tibble(X1 = seq(min(data_sim$X1), max(data_sim$X1), .01),
                   X2 = exp(X1-5/8) - 3/8)

data_sim %>% ggplot(aes(x = X1,
                        y = X2)) +
  geom_point(aes(color = factor(Y)), alpha = .4) +
  geom_abline(intercept = 0, slope = 1, size = 1) +
  geom_line(data = frontera2, aes(x = X1, y = X2), size = 1, color = "orange") +
  theme_bw() +
  labs(color = "Grupo") +
  scale_x_continuous(limits = c(min(data_sim$X1), max(data_sim$X1))) +
  scale_y_continuous(limits = c(min(data_sim$X2), max(data_sim$X2))) +
  theme(legend.position = "top")

## f ####
g_hat_opt <- function(X, Y) {
  pi1_hat <- sum(Y==1)/length(Y)
  pi2_hat <- sum(Y==2)/length(Y)
    
  mu1_hat <- colMeans(X[Y==1,])
  mu2_hat <- colMeans(X[Y==2,])
  
  sigma_hat_1 <- cov(X[Y==1,]) 
  sigma_hat_2 <- cov(X[Y==2,]) 
  sigmaw_hat <- pi1_hat*sigma_hat_1+pi2_hat*sigma_hat_2
  
  w_hat <- solve(sigmaw_hat) %*% (mu2_hat-mu1_hat)
  
  Y_hat <- rep(NA, length(Y))
  
  for (i in 1:length(Y_hat)) {
    Y_hat[i] <- if_else(t(w_hat) %*% X[i,] < t(w) %*% (mu2_hat+mu1_hat)/2 - log(pi2_hat/pi1_hat), 1, 2)  
  }
  
  Y_hat
}

Y_hat_opt <- g_hat_opt(X,Y)
1 - sum(Y_hat_opt==Y)/nRep # El teórico es 0.1198

# La nueva frontera
fronteraopt <- tibble(X1 = seq(min(data_sim$X1), max(data_sim$X1), .01),
                      X2 = (1.765 + 7/3 * X1) * 3/5)

data_sim %>% ggplot(aes(x = X1,
                        y = X2)) +
  geom_point(aes(color = factor(Y)), alpha = .4) +
  geom_abline(intercept = 0, slope = 1, size = 1) +
  geom_line(data = frontera2, aes(x = X1, y = X2), size = 1, color = "orange") +
  geom_line(data = fronteraopt, aes(x = X1, y = X2), size = 1, color = "red") +
  theme_bw() +
  labs(color = "Grupo") +
  scale_x_continuous(limits = c(min(data_sim$X1), max(data_sim$X1))) +
  scale_y_continuous(limits = c(min(data_sim$X2), max(data_sim$X2))) +
  theme(legend.position = "top")

# Se ve que el límite prioriza al grupo 1 porque P(Y==1) es más grande
# ¿Qué pasa si P(Y==1)=P(Y==2) ?
fronteraopt_equal <- tibble(X1 = seq(min(data_sim$X1), max(data_sim$X1), .01),
                            X2 = (2/3 + 7/3 * X1) * 3/5)

data_sim %>% ggplot(aes(x = X1,
                        y = X2)) +
  geom_point(aes(color = factor(Y)), alpha = .4) +
  geom_abline(intercept = 0, slope = 1, size = 1) +
  geom_line(data = frontera2, aes(x = X1, y = X2), size = 1, color = "orange") +
  geom_line(data = fronteraopt, aes(x = X1, y = X2), size = 1, color = "red") +
  geom_line(data = fronteraopt_equal, aes(x = X1, y = X2), size = 1, color = "blue") +
  theme_bw() +
  labs(color = "Grupo") +
  scale_x_continuous(limits = c(min(data_sim$X1), max(data_sim$X1))) +
  scale_y_continuous(limits = c(min(data_sim$X2), max(data_sim$X2))) +
  theme(legend.position = "top")

# Efectivamente la línea azul pareciera partir los datos más "equitativamente"

# Ejercicio 4.3 ####
sigma2 <- matrix(c(1,0.9,0.9,1),2)
sigma1 <- matrix(c(1,1/2,1/2,1),2)
pi1 <- 3/4
pi2 <- 1/4
mu1 <- matrix(c(1,1/2),2,1)
mu2 <- matrix(c(-1/2,1),2,1)

nRep <- 1000
X <- matrix(NA, nRep, 2)
Y <- matrix(NA, nRep, 1)

set.seed(123)
for (i in 1:nRep) {
  Y[i] <- rbinom(1, 1, pi1) #1 si es grupo 1, 0 si es grupo 2
  X[i,] <- Y[i] * mvrnorm(1, mu1, sigma1) + (1-Y[i]) * mvrnorm(1, mu2, sigma2)
}
Y <- if_else(Y==0, 2, 1)
colMeans(X[Y==1,])
colMeans(X[Y==2,])
cov(X[Y==1,])
cov(X[Y==2,])

g_hat_opt_cuad <- function(X, Y) {
  pi1_hat <- sum(Y==1)/length(Y)
  pi2_hat <- sum(Y==2)/length(Y)
  
  mu1_hat <- colMeans(X[Y==1,])
  mu2_hat <- colMeans(X[Y==2,])
  
  sigma1_hat <- cov(X[Y==1,]) 
  sigma2_hat <- cov(X[Y==2,]) 
  
  print(sigma2_hat)
  Y_hat <- rep(NA, length(Y))
  
  for (i in 1:length(Y_hat)) {
    # El lado izquierdo
    W1 <- t(X[i,]-mu1_hat) %*% solve(sigma1_hat) %*% (X[i,]-mu1_hat) - log(pi1_hat)
    # El lado derecho
    W2 <- t(X[i,]-mu2_hat) %*% solve(sigma2_hat) %*% (X[i,]-mu2_hat) - log(pi2_hat)
    
    Y_hat[i] <- if_else(W1 < W2, 1, 2)  
  }
  
  Y_hat
}

Y_hat_opt <- g_hat_opt_cuad(X,Y)
1 - sum(Y_hat_opt==Y)/nRep 

colnames(X) <- c("X1", "X2")

data_sim <- as_tibble(X) %>% 
  cbind(as_tibble(Y)) 

data_sim %>% ggplot(aes(x = X1,
                        y = X2,
                        color = factor(Y))) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "top")

# Ejercicio 4.4 ####
data_cangrejos <- read_delim(here("visualización/TPs/TP4/data/cangrejos.txt"),
                             col_names = c("group", "FL", "RW", "CL", "CW", "BD"))
data_cangrejos

ggpairs(data_cangrejos[, -1], aes(colour = as.factor(data_cangrejos$group), alpha = 0.4))

## a ####
LDA <- lda(formula = group ~ FL + RW + CL + CW + BD,
           data = data_cangrejos)
LDA

prediction <- predict(object = LDA, newdata = data_cangrejos[,-1],  method = "predictive")

data_cangrejos <- data_cangrejos %>%
  bind_cols(tibble(predict = prediction$class))

# Los errores ingenuos serías:
sum(data_cangrejos$predict != data_cangrejos$group)/nrow(data_cangrejos)
# Bastante bueno

clasif <- rep(NA, nrow(data_cangrejos))
for (i in 1:nrow(data_cangrejos)) {
  LDA <- lda(formula = group ~ FL + RW + CL + CW + BD,
             data = data_cangrejos[-i,])
  clasif[i] <- as.numeric(predict(object = LDA, newdata = data_cangrejos[i,-1],  method = "predictive")$class)
}

data_cangrejos <- data_cangrejos %>%
  bind_cols(tibble(predict_loocv = clasif))

# Los errores LOOCV serías:
sum(data_cangrejos$predict_loocv != data_cangrejos$group)/nrow(data_cangrejos)

## b ####
# Repetimos pero con QDA
QDA <- qda(formula = group ~ FL + RW + CL + CW + BD,
           data = data_cangrejos)
QDA

prediction <- predict(object = QDA, newdata = data_cangrejos[,-1],  method = "predictive")

data_cangrejos <- data_cangrejos %>%
  bind_cols(tibble(predict_QDA = prediction$class))

# Los errores ingenuos serías:
sum(data_cangrejos$predict_QDA != data_cangrejos$group)/nrow(data_cangrejos)
# Bastante bueno

clasif <- rep(NA, nrow(data_cangrejos))
for (i in 1:nrow(data_cangrejos)) {
  QDA <- qda(formula = group ~ FL + RW + CL + CW + BD,
             data = data_cangrejos[-i,])
  clasif[i] <- as.numeric(predict(object = QDA, newdata = data_cangrejos[i,-1],  method = "predictive")$class)
}

data_cangrejos <- data_cangrejos %>%
  bind_cols(tibble(predict_loocv_QDA = clasif))

# Los errores LOOCV serías:
sum(data_cangrejos$predict_loocv_QDA != data_cangrejos$group)/nrow(data_cangrejos)

# Ejercicio 4.5 ####
## a ####
data_cangrejos <- read_delim(here("visualización/TPs/TP4/data/cangrejos.txt"),
                             col_names = c("group", "FL", "RW", "CL", "CW", "BD"))

LDA <- lda(formula = group ~ FL + RW + CL + CW + BD,
           data = data_cangrejos)

X <- as.matrix(data_cangrejos[,-1]) %*% LDA$scaling

data_plot <- as_tibble(X) %>%
  mutate(group = data_cangrejos$group)

data_plot %>% ggplot(aes(x = LD1,
           y = LD2,
           color = as.factor(group))) +
  geom_point() +
  theme_bw() +
  labs(color = "Grupo") +
  theme(legend.position = "top")

data_fondo <- expand_grid(FL = seq(min(data_cangrejos$FL),max(data_cangrejos$FL),2),
                          RW = seq(min(data_cangrejos$RW),max(data_cangrejos$RW),2),
                          CL = seq(min(data_cangrejos$CL),max(data_cangrejos$CL),2),
                          CW = seq(min(data_cangrejos$CW),max(data_cangrejos$CW),2),
                          BD = seq(min(data_cangrejos$BD),max(data_cangrejos$BD),2))


prediction <- as.numeric(predict(object = LDA, newdata = data_fondo,  method = "predictive")$class)

data_plot_fondo <- as_tibble(as.matrix(data_fondo) %*% LDA$scaling) %>%
  mutate(group = prediction)

data_plot %>% ggplot(aes(x = LD1,
                         y = LD2,
                         color = as.factor(group))) +
  geom_point(data = data_plot_fondo, alpha = .1, size = 3) +
  geom_point() +
  theme_bw() +
  labs(color = "Grupo") +
  scale_x_continuous(limits = c(min(data_plot$LD1), max(data_plot$LD1))) +
  scale_y_continuous(limits = c(min(data_plot$LD2), max(data_plot$LD2))) +
  theme(legend.position = "top")

## con QDA ####
QDA <- qda(formula = group ~ FL + RW + CL + CW + BD,
           data = data_cangrejos)

X <- rbind(as.matrix(data_cangrejos[data_cangrejos[,1]==1,-1]) %*% QDA$scaling[,,1][,1:2],
           as.matrix(data_cangrejos[data_cangrejos[,1]==2,-1]) %*% QDA$scaling[,,2][,1:2],
           as.matrix(data_cangrejos[data_cangrejos[,1]==3,-1]) %*% QDA$scaling[,,3][,1:2])

data_plot <- as_tibble(X) %>%
  mutate(group = data_cangrejos$group) %>%
  rename(QD1 = `1`) %>%
  rename(QD2 = `2`)

data_plot %>% ggplot(aes(x = QD1,
                         y = QD2,
                         color = as.factor(group))) +
  geom_point() +
  theme_bw() +
  labs(color = "Grupo") +
  theme(legend.position = "top")

data_fondo <- expand_grid(FL = seq(min(data_cangrejos$FL),max(data_cangrejos$FL),2),
                          RW = seq(min(data_cangrejos$RW),max(data_cangrejos$RW),2),
                          CL = seq(min(data_cangrejos$CL),max(data_cangrejos$CL),2),
                          CW = seq(min(data_cangrejos$CW),max(data_cangrejos$CW),2),
                          BD = seq(min(data_cangrejos$BD),max(data_cangrejos$BD),2))


prediction <- as.numeric(predict(object = QDA, newdata = data_fondo,  method = "predictive")$class)

X_fondo <- rbind(as.matrix(data_fondo[prediction==1,]) %*% QDA$scaling[,,1][,1:2],
                 as.matrix(data_fondo[prediction==2,]) %*% QDA$scaling[,,2][,1:2],
                 as.matrix(data_fondo[prediction==3,]) %*% QDA$scaling[,,3][,1:2])

data_plot_fondo <- as_tibble(X_fondo) %>%
  mutate(group = prediction) %>%
  rename(QD1 = `1`) %>%
  rename(QD2 = `2`)

data_plot %>% ggplot(aes(x = QD1,
                         y = QD2,
                         color = as.factor(group))) +
  geom_point(data = data_plot_fondo, alpha = .1, size = 3) +
  geom_point() +
  theme_bw() +
  labs(color = "Grupo") +
  scale_x_continuous(limits = c(min(data_plot$QD1), max(data_plot$QD1))) +
  scale_y_continuous(limits = c(min(data_plot$QD2), max(data_plot$QD2))) +
  theme(legend.position = "top")
