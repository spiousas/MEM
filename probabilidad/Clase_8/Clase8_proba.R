hist(runif(10000,0,1))

hist(qexp(runif(10000,0,1)))

hist(sqrt(runif(10000,0,1)))

exp(-Inf)

# Ejercicio 3 ####

rejercicio <- function(N) {
  salida <- matrix(NA, N, 2)
  for (i in 1:N) {
    x <- sqrt(runif(1, 0, 1))
    y <- rexp(1, 1/x^2)
    salida[i,] <- c(x,y)  
  }
  salida
}

datos_prueba <- rejercicio(1e4)
mean(datos_prueba[,1])
mean(datos_prueba[,2])
mean(datos_prueba[,1]*datos_prueba[,2])

cov(datos_prueba)

