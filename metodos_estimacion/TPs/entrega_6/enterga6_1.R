pacman::p_load(tidyverse, here)

data <- scan()

# a ####
n <- length(data)
probabilidad <- 1/n
probabilidad

# b ####
esperanza <- mean(data)
esperanza

# c ####
varianza <- mean((data-mean(data))^2)
varianza

# d ####
Fx <- ecdf(data)
Fx(3.2)

# Plot ####
plot(Fx, main = "Función de distribución acumulada", ylab = "F(x)" )
grid()
