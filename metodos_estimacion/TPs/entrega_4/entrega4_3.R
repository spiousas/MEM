# Ejercicio 9 ####
x <- scan()

intervalo_lambda_exacto <- function(alpha, x) {
  n <- length(x)
  ci_low <- qchisq(df = 2*n, p = alpha/2) / (2*mean(x)*n)
  ci_up <- qchisq(df = 2*n, p = 1-alpha/2) / (2*mean(x)*n)
  c(ci_low, ci_up)
}

cubrimiento_empirico_lambda <- function(lambda, nivel, n, Nrep, seed) {
  set.seed(seed)
  alpha <- 1-nivel
  cubrimiento <- rep(NA, Nrep)
  for (i in 1:Nrep) {
    x <- rexp(n = n, rate = lambda)
    ci <- intervalo_lambda_exacto(alpha = alpha, 
                                  x = x)
    cubrimiento[i] <- (lambda>ci[1]) & (lambda<ci[2])
  }
  mean(cubrimiento)
}

cubrimiento_empirico_lambda(lambda = 1,
                            nivel = 0.95,
                            n = 10,
                            Nrep = 100000,
                            seed = 1)

## a ####
nivel <- 0.94
alpha <- 1-nivel

intervalo_lambda_exacto(alpha = alpha, x = x)

## b ####
rev(1 / intervalo_lambda_exacto(alpha = alpha,x = x))
# Los doy vuelta porque al invertir se dan vuelta las desigualdades
