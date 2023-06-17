# Ejercicio 9 ####
x <- scan()

# Funciones ####
intervalo_mu_exacto <- function(nivel, desvio, x) {
  n <- length(x)
  if (is.null(desvio)) {
    ci_low <- mean(x)+qt(df = n-1, p = nivel/2)*sd(x)*sqrt(1/n)
    ci_up <- mean(x)+qt(df = n-1, p = 1-nivel/2)*sd(x)*sqrt(1/n)
  } else {
    ci_low <- mean(x)+qnorm(p = nivel/2)*desvio*sqrt(1/n)
    ci_up <- mean(x)+qnorm(p = 1-nivel/2)*desvio*sqrt(1/n)
  }
  c(ci_low, ci_up)
}

intervalo_sd2_exacto <- function(nivel, x) {
  n <- length(x)
  ci_low <- (n-1)*sd(x)^2/(qchisq(p = 1-nivel/2, df = n-1))
  ci_up <- (n-1)*sd(x)^2/(qchisq(p = nivel/2, df = n-1))
  c(ci_low, ci_up)
}


intervalo_sd_exacto <- function(nivel, x) {
  n <- length(x)
  ci_low <- (n-1)*sd(x)^2/(qchisq(p = 1-nivel/2, df = n-1))
  ci_up <- (n-1)*sd(x)^2/(qchisq(p = nivel/2, df = n-1))
  c(sqrt(ci_low), sqrt(ci_up))
}

## a ####
intervalo_mu_exacto(nivel = 0.01, 
                    desvio = NULL, 
                    x = x)

## c ####
intervalo_sd2_exacto(nivel = 0.01, 
                     x = x)
## c ####
intervalo_sd_exacto(nivel = 0.01, 
                    x = x)
