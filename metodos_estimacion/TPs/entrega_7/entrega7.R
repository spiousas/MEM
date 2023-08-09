pacman::p_load(tidyverse, here)

# Leo los datos y los convierto en un tibbel
data <- tibble(read.table("metodos_estimacion/TPs/entrega_7/data/data_p_7.txt", header = T))
data

# La cantidad de filas
n <- nrow(data)
p <- 2

# Busco la matriz A y los autovectores y autovalores
A <- solve(cov(data))
eigenvalues <- eigen(A)$values
eigenvectors <- eigen(A)$vectors

# Defino el vector de ángulos entre 0 y 2*pi con 100 pasos
theta <- seq(0,2*pi,2*pi/100)

# El alpha y el F_alpha
alpha <- .05
f_alpha <- qf(1-alpha, p, n-p)

# Calculo c
c <- sqrt(p*(n-1)/(n*(n-p))*f_alpha)

# Calculo el elipsoide
x <- vector()
y <- vector()
for (i in 1:length(est)){
  ro1 <- (c/sqrt(eigenvalues[1]))*cos(theta[i])
  ro2 <- (c/sqrt(eigenvalues[2]))*sin(theta[i])
  V <- eigenvectors %*% c(ro1,ro2)
  x[i] <- V[1]
  y[i] <- V[2]
}

# Centro el elipsoide
x_centered <- x + mean(data$Peso)
y_centered <- y + mean(data$Altura)

# Armo un tibble
elipsoide <- tibble(theta = theta,
                    x = x_centered,
                    y = y_centered)

# Visualizo el elipsoide
elipsoide %>% ggplot(aes(x = x,
           y = y)) +
  geom_path(linewidth = 1, color = "#85C7F2", alpha = .5) +
  geom_point(size = 2, color = "#85C7F2") +
  geom_point(x = mean(data$Peso), y = mean(data$Altura), size = 3, color = "#636363") + 
  labs(x = "Peso [Kg]", y = "Altura [cm]",
       title = "Elipsoide de confianza de nivel 95%") +
  theme_bw()
  