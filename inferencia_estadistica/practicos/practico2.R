# Ejercicio 1 ####

## La definición del test ####
testNP <- function(x, mu0, sigma0, alpha = 0.05, direction) {
  # Direction es la direccion de la H1, si es mayor o menor
  t <- sum((x-mu0)^2/sigma0^2)
  if (direction == "menor") {
    cuantil <- qchisq(df = length(x), p = alpha)
    test <- t < cuantil
    p_valor <- pchisq(df = length(x), q = t)
  } else if (direction == "mayor") {
    cuantil <- qchisq(df = length(x), p = 1-alpha)
    test <- t > cuantil
    p_valor <- 1 - pchisq(df = length(x), q = t)
  } else {
    stop("Non-valid direction!")
  }
  return(c(test, p_valor))
}

## Probemos que ande ####
mu0 <- 0
sigma0 <- 1
sigma <- 0.6
alfa <- .05
n <- 21
x <- rnorm(n = n, mean = mu0, sd = sigma)

testNP(x = x, mu0 = mu0, sigma0 = sigma0, alpha = alfa, direction = "menor")

## A ver cómo da el error de tipo I en el long run ####
Nrep <- 1e4
test_res <- rep(NA, Nrep)
for (i in 1:Nrep) {
  x <- rnorm(n = n, mean = mu0, sd = sigma)
  test_res[i] <- testNP(x = x, mu0 = mu0, sigma0 = sigma0, alpha = .05, direction = "la")
}
mean(test_res)

## Comparemos la potencia teórica con la empírica (para menor)
sigmas <- seq(0, 2, 0.01)
pot_t <- pchisq(df = n, q = (sigma0/sigmas)^2*qchisq(df = n, p = alfa) )
plot(sigmas, pot_t)
pot_t[sigmas == 1]


Nrep <- 1e4
test_res <- rep(NA, Nrep)
pot_emp <- rep(NA, length(sigmas))
for (i in 1:length(sigmas)) {
  for (j in 1:Nrep) {
    x <- rnorm(n = n, mean = mu0, sd = sigmas[i])
    test_res[j] <- testNP(x = x, mu0 = mu0, sigma0 = sigma0, alpha = alfa, direction = "menor")[1]
  }
  pot_emp[i] <- mean(test_res)
}

plot(sigmas, pot_t)
lines(sigmas, pot_emp, col = "red")

## Comparemos la potencia teórica con la empírica (para mayor)
sigmas <- seq(0, 2, 0.01)
pot_t <- 1 - pchisq(df = n, q = (sigma0/sigmas)^2*qchisq(df = n, p = 1-alfa) )
plot(sigmas, pot_t)
pot_t[sigmas == 1]


Nrep <- 1e4
test_res <- rep(NA, Nrep)
pot_emp <- rep(NA, length(sigmas))
for (i in 1:length(sigmas)) {
  for (j in 1:Nrep) {
    x <- rnorm(n = n, mean = mu0, sd = sigmas[i])
    test_res[j] <- testNP(x = x, mu0 = mu0, sigma0 = sigma0, alpha = alfa, direction = "mayor")[1]
  }
  pot_emp[i] <- mean(test_res)
}

plot(sigmas, pot_t)
lines(sigmas, pot_emp, col = "red")

# Ejercicio 2 ####

## a ####
# La región de rechazo es para mu<= mu0

testNP_mu <- function(x, mu0, sigma0, alpha = 0.05, direction) {
  # Direction es la direccion de la H1, si es mayor o menor
  n <- length(x)
  t <- sqrt(n)*(mean(x)-mu0)/sigma0
  if (direction == "menor") {
    cuantil <- qnorm(p = alpha)
    test <- t < cuantil
    p_valor <- pnorm(q = t)
  } else if (direction == "mayor") {
    cuantil <- qnorm(p = 1-alpha)
    test <- t > cuantil
    p_valor <- pnorm(q = t)
  } else {
    stop("Non-valid direction!")
  }
  return(c(test, p_valor))
}

## Probemos que ande ####
mu0 <- 1
sigma0 <- 2
mu <- 0
alfa <- .05
n <- 10
x <- rnorm(n = n, mean = mu, sd = sigma0)

testNP_mu(x = x, mu0 = mu0, sigma0 = sigma0, alpha = alfa, direction = "mayor")

## Comparemos la potencia teórica con la empírica (para mayor)
mus <- seq(-1, 3, 0.1)
pot_t <- 1 - pnorm(q = qnorm(1-alfa) + (mu0-mus)/(sigma0/sqrt(n)))
plot(mus, pot_t)
pot_t[mus == 1]

Nrep <- 1e4
test_res <- rep(NA, Nrep)
pot_emp <- rep(NA, length(mus))
for (i in 1:length(mus)) {
  for (j in 1:Nrep) {
    x <- rnorm(n = n, mean = mus[i], sd = sigma0)
    test_res[j] <- testNP_mu(x = x, mu0 = mu0, sigma0 = sigma0, alpha = alfa, direction = "mayor")[1]
  }
  pot_emp[i] <- mean(test_res)
}

plot(mus, pot_t)
lines(mus, pot_emp, col = "red")

# Ejercicio 3 ####
# La función del test es la misma que en el ejercicio anterior con direction="mayor", mu0=0 y sigma0=1

Nrep <- 1e4
test_p <- rep(NA, Nrep)
test_res <- rep(NA, Nrep)
pot_emp <- rep(NA, length(mus))

mu0 <- 0
sigma0 <- 1
mu <- 0
alfa <- .05
n <- 20

# Los "alfas" empiricos ####
Ntry <- 300
p_valores <- rep(NA, length(Ntry))
# Test común
for (i in 1:Ntry) {
  for (j in 1:Nrep) {
    x <- rnorm(n = n, mean = mu, sd = sigma0)
    test_res[j] <- testNP_mu(x = x, mu0 = mu0, sigma0 = sigma0, alpha = alfa, direction = "mayor")[1]  
  }  
  p_valores[i] <- mean(test_res)
}

hist(p_valores,xlim=c(0,0.15), freq = F)

# El test secuencial
p_valores <- rep(NA, length(Ntry))
for (i in 1:Ntry) {
  for (j in 1:Nrep) {
    x <- rnorm(n = n, mean = mu, sd = sigma0)
    test_res[j] <- testNP_mu(x = x, mu0 = mu0, sigma0 = sigma0, alpha = alfa, direction = "mayor")[1]  
    if (test_res[j] == 0) {
      x <- c(x, rnorm(n = 10, mean = mu, sd = sigma0))
      test_res[j] <- testNP_mu(x = x, mu0 = mu0, sigma0 = sigma0, alpha = alfa, direction = "mayor")[1]  
    }
  }  
  p_valores[i] <- mean(test_res)
}

hist(p_valores,xlim=c(0,0.15), freq = F)

# El alfa real del test
mean(p_valores)

## La potencia ####
#mus <- seq(-1, 1, 0.1)
mus <- c(0.5, 1, 2, 5, 10)
pot_emp <- rep(NA, length(mus))
for (i in 1:length(mus)) {
  for (j in 1:Nrep) {
    x <- rnorm(n = n, mean = mus[i], sd = sigma0)
    test_res[j] <- testNP_mu(x = x, mu0 = mu0, sigma0 = sigma0, alpha = alfa, direction = "mayor")[1]  
    if (test_res[j] == 0) {
      x <- c(x, rnorm(n = 10, mean = mus[i], sd = sigma0))
      test_res[j] <- testNP_mu(x = x, mu0 = mu0, sigma0 = sigma0, alpha = alfa, direction = "mayor")[1]  
    }
  }  
  pot_emp[i] <- mean(test_res)
}
pot_emp[mus==0]
plot(mus, pot_emp)

# Ejercicio 1 ####
test_asint_Ber <- function(x, p0, alpha = 0.05, direction) {
  # Direction es la direccion de la H1, si es mayor o menor
  n <- length(x)
  t <- sqrt(n) * (mean(x)-p0) / sqrt(p0*(1-p0))
  if (direction == "menor") {
    cuantil <- qnorm(p = alpha)
    test <- t < cuantil
  } else if (direction == "mayor") {
    cuantil <- qnorm(p = 1-alpha)
    test <- t > cuantil
  } else {
    stop("Non-valid direction!")
  }
  return(test)
}

n <- 10
p0 <- 0.5
p <- .5

x <- rbinom(n = n, size = 1, prob = p)
test_asint_Ber(x, p0, alpha = 0.05, direction = "mayor")

# Simulo las potencias
ns <- c(10, 70, 200)
p0s <- c(.5, .8, .97)
ps <- c(0.03, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.97)

pot_emp_Ber <- function(n, p0, p, alfa = .05, Nrep) {
  test_res <- rep(NA, Nrep)
  for (i in 1:Nrep) {
    x <- rbinom(n = n, size = 1, prob = p)
    test_res[i] <- test_asint_Ber(x, p0, alpha = alfa, direction = "mayor")
  }
  mean(test_res)
}

data <- expand_grid(n = ns, p0 = p0s, p = ps) %>%
  rowwise() %>%
  mutate(pot = pot_emp_Ber(n=n, p0=p0, p=p, alfa = .05, Nrep = 1e3))

data %>% ggplot(aes(x = p,
           y = pot,
           color = factor(p0))) +
  geom_line() +
  geom_vline(aes(xintercept = p0, color = factor(p0))) +
  geom_hline(yintercept = 0.05) +
  facet_grid(n~.) +
  theme_minimal()
  
