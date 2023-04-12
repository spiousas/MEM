library(tidiverse)

# Ejercicio 9 ####
# b ####
fun_b <- function(x) {
  exp(x)
}

n <- 1:1000
sumas <- n
for (n_i in n) {
  suma <- 0
  for (i in seq(1,n_i)) {
    suma <- suma + fun_b(runif(n = 1))
  }
  sumas[n_i] <- 1/n_i * suma
}
plot(n, sumas)
lines(n, rep(x = exp(1)-1, length(n)), col = "red")

# d ####
fun_d <- function(x) {
  exp(-x^2/2)
}

# La integral
int <- 0 
dx <- .0001
x_v <- seq(0, 1, dx)
for (x in x_v) {
  int <- int + fun_d(x) * dx  
}
int

# La aproximación 
n <- 1:1000
sumas <- n
for (n_i in n) {
  suma <- 0
  for (i in seq(1,n_i)) {
    suma <- suma + fun_d(runif(n = 1))
  }
  sumas[n_i] <- 1/n_i * suma
}
plot(n, sumas)
lines(n, rep(x = int, length(n)), col = "red")



