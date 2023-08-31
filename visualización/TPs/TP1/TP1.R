pacman::p_load(here, tidyverse, matlib, MASS, plotly)

# Ejercicio 1.1 ####

## a ####
data_pbi <- read_csv(here("visualización/TPs/data/PBI.csv"))
x <- as.matrix(data_pbi)

## b ####
pairs(x, pch=20)  

## c ####
media_muestral <- colMeans(x)

n <- nrow(x)
I <- diag(rep(1, n))
unos <- matrix(1, n, 1)

H <- I - 1/n * unos %*% t(unos)
x_cent <- H %*% x

pairs(x_cent, pch = 20)

## d ####
Q <- t(x_cent) %*% x_cent
Q # Es la matriz de suma de cuadrados
s <- 1/(n-1) * Q # Es la matriz de covarianza

## e ####
D_12 <- diag(sqrt(1/diag(s)))
D_12

Z <- H %*% x %*% D_12 # Matriz de datos estandarizada

pairs(Z)
cov(Z)

# Ejercicio 1.2 ####
## a ####
A <- matrix(c(4, 5, 4, 2), nrow = 2, ncol = 2)
A

## b ####
mu_x <- matrix(c(10, 4), nrow = 2, ncol = 1)
mu_x
mu_y <- A %*% mu_x # Porque E(Ax) = A E(x)
mu_y

cov_x <- matrix(c(5, -1, -1, 1/2), nrow = 2, ncol = 2)
cov_x
cov_y <- A %*% cov_x %*% t(A) # Porque cov(y,y) = cov(Ax, Ax) = A cov(x,x) t(A)
cov_y

# Y1 e Y2 no son independientes sino sus covarianzas (los elementos fuera de la 
# diagonal de cov_y) serían 0. Además, son combinaciones lineales de X1 y X2.

## c ####
data_placas<- read_delim(here("visualización/TPs/TP1/data/placas.txt"),delim = "\t")
x_placas <- as.matrix(data_placas)

colMeans(x_placas)
cov(x_placas)

A_datos <- t(A) # Es la traspuesta de la A que usamos para los vectores aleatorios
A_datos

y_placas <- x_placas %*% A_datos # En lugar d epremultiplicar se posmultiplica por la traspuesta
colnames(y_placas) <- c("costo", "venta")
colMeans(y_placas)

## d ####
cov(x_placas)
cor(x_placas)
cov(y_placas) # No son independientes sino su covarianza sería 0
cor(y_placas)

# Ejercicio 1.3 ####
# Vectores ejemplo
x <- matrix(rnorm(36), ncol=2)
x

m <- matrix(rnorm(2), nrow=2)
m

cross <- function(x, m) {
  suma <- 0
  n <- nrow(x)
  
  for (i in 1:n) {
    suma <- suma + (x[i,]-m) %*% t(x[i,]-m)
    # Aca como x es una fila m es una columna
  }
  
  1/(n-1) * suma
}

# Probamos que ande
cross(x,m)

## a ####
# Comparamos con la cov(.)
cov(x)
# Para que la cuenta de arriba se convierta en al matriz de covarianza m debe 
# ser el vector media muestra Xraya
cross(x, colMeans(x))

## b ####
cor(x)
# Para que sea la correlación tengo que multiplicar por una matriz que tiene
# Sii^(1/2) en la diagonal y 0s en fuera de la diagonal. Es decir, al hacer eso
# y que el vector m sea las medias estaría estandarizando.
cross(x %*% diag(sqrt(1/diag(cov(x)))), colMeans(x))

# Ejercicio 1.4 ####
# Vectores ejemplo
x <- matrix(rnorm(36), ncol=2)
x

m <- matrix(rnorm(2), nrow=2)
m

M <- diag(2)
M
i <- 1

d <- sqrt(t(x[i,]-m) %*% M %*% (x[i,]-m))

distance <- function(x, m, M) {                                                                               
  for (i in 1:nrow(x)) {
    d[i] <- sqrt(t(x[i,]-m) %*% inv(M) %*% (x[i,]-m))
  }
  
  matrix(d)
}

distance(x, m, M)

## a ####
# Justamente si M es la diagonal, d se convierte en la distancia euclidea.
# Hagamos a mano para i=1
sqrt((x[1,1]-m[1])^2 + (x[1,2]-m[2])^2)
distance(x, m, M)[1]

## b ####
M <- diag(diag(cov(x)))
M
distance(x, m, M)
# Lo que me devuelve es la distancia en cantidad de desviaciones estándar

## c ####
M <- cov(x)
M
distance(x, m, M)


# Ejercicio 1.5 ####
set.seed(1234)
x <- mvrnorm(n = 50, mu = c(0,0), Sigma = matrix(c(2,1,1,2), nrow = 2))
data <- as_tibble(x, .name_repair = "universal") %>%
  rename(x1 = "...1", x2 = "...2")

# a <- matrix(c(1, 1), nrow = 2)
a <- matrix(c(cos(pi/4), cos(pi/4)), nrow = 2)  

# La x proyectada en la recta y después puesta en R2 es x %*% a %*% t(a)
data_rot <- as_tibble(x %*% a %*% t(a), .name_repair = "universal") %>% 
  rename(x1 = "...1", x2 = "...2") 
data_rot

data %>% ggplot(aes(x = x1,
                    y = x2)) +
  geom_point(color = "grey") + 
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(data = data_rot, color = "red") +
  scale_x_continuous(limits = c(-4,4), minor_breaks = NULL) +
  scale_y_continuous(limits = c(-4,4), minor_breaks = NULL) +
  theme_bw() 

# Interpretación con componentes principales
# t(A) * x = psi
# Poblacional ->
# A = [b1, b2] donde b1 y b2 son los autovectores asosiados a los autovalores
# lambda1 >= lambda2 de la matriz Sigma
# Muestral ->
# x * Ahat = Z
# Ahat = [v1_hat, v2_hat] donde v1_hat y v2_hat son los autovectores asosiados
# a los autovalores lambda1 >= lambda2 de la matriz S (la estimada de Sigma)
# Z es psi_hat

Sigma <- matrix(c(2,1,1,2), nrow = 2)
A <- eigen(Sigma)$vectors
A_hat <- eigen(cov(x))$vectors

data_PCA_data <- as_tibble(x %*% A_hat, .name_repair = "universal") %>% 
  rename(x1 = "...1", x2 = "...2") 

data %>% ggplot(aes(x = x1,
                    y = x2)) +
  geom_point(color = "grey") + 
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_abline(slope = 1, intercept = 0) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(data = A_hat[2,1]/A_hat[1,1], color = "red") +
  scale_x_continuous(limits = c(-4,4), minor_breaks = NULL) +
  scale_y_continuous(limits = c(-4,4), minor_breaks = NULL) +
  theme_bw() + theme(aspect.ratio=1)

eigen(cov(x))$values/sum(eigen(cov(x))$values)

# Ejercicio 1.6 ####
## a ####
a1 <- matrix(c(cos(pi/4), cos(pi/4)), nrow = 2)  
mu <- matrix(c(0,0), nrow = 2)
Sigma = matrix(c(2,1,1,2), nrow = 2)

EY1 <- t(a1) %*% mu
EY1
SigmaY1 <- t(a1) %*% Sigma %*% a1
SigmaY1

Y <- as_tibble(x %*% a, .name_repair = "universal") %>% 
  rename(y1 = "...1") 

## b ####
Y %>% ggplot(aes(x = y1)) +
  geom_histogram(aes(y = after_stat(..density..))) + 
  geom_density() + 
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = sqrt(3)), color = "red") +
  theme_bw()

## c ####
a2 <- matrix(c(-cos(pi/4), cos(pi/4)), nrow = 2)  

EY2 <- t(a2) %*% mu
EY2
SigmaY2 <- t(a2) %*% Sigma %*% a2
SigmaY2

Y <- Y %>% mutate(y2 = x %*% a2) 

# Puedo obtener la matriz Y multiplicando directamente por una matriz a
a <- cbind(a1, a2)
Y_prima <- as_tibble(x %*% a, .name_repair = "universal") %>%
  rename(y1 = "...1", y2 = "...2")
Y_prima

# Ploteo Y e Y_prima en el espacio rotado
Y %>% ggplot(aes(x = y1, y = y2)) +
  geom_point(data = Y_prima, color = "red", shape = 21, size = 3, fill = "white") +
  geom_point() +
  theme_bw()
# Obtengo el mismo resultado de ambas maneras

Y %>% ggplot(aes(x = y2)) +
  geom_histogram(aes(y = after_stat(..density..))) + 
  geom_density() + 
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = sqrt(1)), color = "red") +
  theme_bw()

# Si quiero tener una expresión general de Sigma_y la debería multiplicar por a
SigmaY <- t(a) %*% Sigma %*% a
SigmaY

# En este caso la diagonal principal corresponde con las varianzas que ya había calculado
# y los elementos fuera de la diagonal dan cero. Esto ocurre porque las direcciones 
# propuestas en el ejercicio son autovectores de Sigma, es decir, "Y" es la  proyección de 
# x e unos ejes rotados de forma tan que no están correlacionadas (covarianza=0).

## d ####
# La x proyectada en la recta y después puesta en R2 es x %*% a %*% t(a)
data_rot_2 <- as_tibble(x %*% a2 %*% t(a2), .name_repair = "universal") %>% 
  rename(x1 = "...1", x2 = "...2") 
data_rot_2

data %>% ggplot(aes(x = x1,
                    y = x2)) +
  geom_point(color = "grey") + 
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_abline(slope = 1, intercept = 0) +
  geom_abline(slope = -1, intercept = 0) +
  geom_point(data = data_rot, color = "red") +
  geom_point(data = data_rot_2, color = "red") +
  scale_x_continuous(limits = c(-4,4), minor_breaks = NULL) +
  scale_y_continuous(limits = c(-4,4), minor_breaks = NULL) +
  theme_bw() + theme(aspect.ratio=1)

## e ####
Y %>% ggplot(aes(x = y1,
                 y = y2)) +
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_x_continuous(limits = c(-4,4), minor_breaks = NULL) +
  scale_y_continuous(limits = c(-4,4), minor_breaks = NULL) +
  theme_bw() + theme(aspect.ratio=1)

cov(Y)
cor(Y)

sum(diag(cov(x)))
sum(diag(cov(Y)))

## f ####
a2 <- matrix(c(1/sqrt(5), 2/sqrt(5)), nrow = 2)  

EY2 <- t(a2) %*% mu
EY2
SigmaY2 <- t(a2) %*% Sigma %*% a2
SigmaY2

Y <- as_tibble(x %*% a, .name_repair = "universal") %>% 
  rename(y1 = "...1") 
Y <- Y %>% mutate(y2 = x %*% a2) 

Y %>% ggplot(aes(x = y2)) +
  geom_histogram(aes(y = after_stat(..density..))) + 
  geom_density() + 
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = sqrt(1)), color = "red") +
  theme_bw()

# La x proyectada en la recta y después puesta en R2 es x %*% a %*% t(a)
data_rot_2 <- as_tibble(x %*% a2 %*% t(a2), .name_repair = "universal") %>% 
  rename(x1 = "...1", x2 = "...2") 
data_rot_2

data %>% ggplot(aes(x = x1,
                    y = x2)) +
  geom_point(color = "grey") + 
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_abline(slope = a1[2]/a1[1], intercept = 0) +
  geom_abline(slope = a2[2]/a2[1], intercept = 0) +
  geom_point(data = data_rot, color = "red") +
  geom_point(data = data_rot_2, color = "red") +
  scale_x_continuous(limits = c(-4,4), minor_breaks = NULL) +
  scale_y_continuous(limits = c(-4,4), minor_breaks = NULL) +
  theme_bw() + theme(aspect.ratio=1)

## e ####
Y %>% ggplot(aes(x = y1,
                 y = y2)) +
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_x_continuous(limits = c(-4,4), minor_breaks = NULL) +
  scale_y_continuous(limits = c(-4,4), minor_breaks = NULL) +
  theme_bw() + theme(aspect.ratio=1)

cov(Y)
cor(Y)

sum(diag(cov(x)))
sum(diag(cov(Y)))

# La suma de las diagonales de las matrices de covarianza son iguales

# Ejercicio 1.7 ####

data_Hg <- read_csv(here("visualización/TPs/data/mercurio.csv"))

## a ####
fig <- plot_ly(data_Hg, 
               x = ~alcalinidad, 
               y = ~clorofila, 
               z = ~mercurio, 
               color = ~etiqueta, 
               colors = c('#BF382A', '#0C4B8E'))

fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Alcalinidad'),
                                   yaxis = list(title = 'Clorofila'),
                                   zaxis = list(title = 'Mercurio')))
fig

## b ####
data_Hg <- data_Hg %>%
  mutate(w1 = sqrt(alcalinidad),
         w2 = sqrt(clorofila),
         w3 = log(mercurio))

fig <- plot_ly(data_Hg, 
               x = ~w1, 
               y = ~w2, 
               z = ~w3, 
               color = ~etiqueta, 
               colors = c('#BF382A', '#0C4B8E'))

fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'W1'),
                                   yaxis = list(title = 'W2'),
                                   zaxis = list(title = 'W3')))
fig

## c ####
# Matriz de correlación de los datos originales
X <- as.matrix(data_Hg[,1:3])
cor(X)
# Matriz de correlación de los datos transformados
W <- as.matrix(data_Hg[,5:7])
sigmaW <- cor(W)
cor(W)

## d ####
A <- matrix(c(-3/4, -2/3, 1/5, 2/3, -4/5, -1/20), nrow = 3)
A

Y <- as_tibble(W %*% A, .name_repair = "universal") %>%
  rename(y1 = "...1", y2 = "...2")
Y

Y %>% ggplot(aes(x = y1, y = y2)) +
  geom_point() +
  theme_bw()

cov(Y)

## e ####
# Medias por variable
mediaW <- t(as.matrix(colMeans(W)))
# Distancia de Mahalanobis
dist_M <- rep(NA, nrow(W))
# Distancia Euclidea
dist_E <- rep(NA, nrow(W))

for (i in 1:nrow(W)) {
  dist_E[i] <- sqrt(norm(W[i,]-mediaW, type="2"))
  dist_M[i] <- (W[i,]-mediaW) %*% inv(sigmaW) %*% t(W[i,]-mediaW)
}

data_Hg <- data_Hg %>% mutate(dist_E = dist_E,
                              dist_M = dist_M)

data_Hg %>% ggplot(aes(x = dist_E,
                       y = dist_M)) +
  geom_point() + 
  theme_bw()
  
