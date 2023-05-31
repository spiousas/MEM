datos <- scan()

# a ####
mu <- mean(datos)
mu

# b #### 
sigma2 <- 1/length(datos) * sum(datos^2) - (1/length(datos)*sum(datos))^2
sigma2

# c ####
S <- sd(datos)
S^2

# d ####
sigma0 <- 2
sqrt(sigma0/length(datos))

# e ####
-sigma0/length(datos)

# f ####
0

# g ####
sqrt(S^2/length(datos))
