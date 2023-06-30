# Ejercicio 6 ####
theta <- 6
n <- 100
x <- runif(n = n, min = -theta, max = theta)

theta_hat <- sqrt(3/n * sum(x*x))
theta_hat

# Ejercicio 7 ####

# Chequear el EMV de la pseudouniforme
theta <- seq(0.1, 5, .01)
max_y <- 2
lx[theta<max_y] <- 0
plot(theta,lx)

# Ahora usamos los estimadores para hacer los calculos
x <- c(1.49, 1.12, 1.08, 0.65, 0.98, 0.86, 0.37, 0.41, 0.91, 0.85, 
       0.25, 0.78, 0.30, 1.41, 0.18, 0.52, 0.40, 0.69, 0.73, 0.81)

theta_x <- sqrt(2/mean(x))
theta_x

y <- c(1.32, 0.59, 1.41, 1.15, 0.34, 1.16, 1.29, 1.25, 1.18, 0.27,
       1.31, 0.96, 0.57, 0.66, 0.67, 1.34, 0.47, 1.07, 1.13, 0.25)

theta_y <- max(y)
theta_y

2/3 * max(y)
mean(y)