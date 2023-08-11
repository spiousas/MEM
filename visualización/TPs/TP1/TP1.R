pacman::p_load(here, tidyverse, matlib, MASS)

# Ejercicio 1 ####

## a ####
data_pbi <- read_csv(here("visualización/TPs/TP1/data/PBI.csv"))
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

# Ejercicio 2 ####
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

# Ejercicio 3 ####
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

# Ejercicio 4 ####
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

# Ejercicio 5 ####
x <- matrix(rnorm(100), ncol=2)
x

set.seed(1234)
x <- mvrnorm(n = 50, mu = c(0,0), Sigma = matrix(c(2,1,1,2), nrow = 2))
data <- as_tibble(x, .name_repair = "universal") %>%
  rename(x1 = "...1", x2 = "...2")

a <- matrix(c(cos(pi/4), cos(pi/4)), nrow = 2)  

data_rot <- as_tibble(x %*% a, .name_repair = "universal") %>%
  rename(mod = "...1") %>%
  mutate(x1 = mod * cos(pi/4),
         x2 = mod * sin(pi/4))
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


