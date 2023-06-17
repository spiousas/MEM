# Ejercicio 1 ####
x <- scan()

## a ####
nivel <- 0.97
var <- 1/2
beta <- 1-nivel

mean(x)+qnorm(beta/2)*sqrt(var/length(x))
mean(x)+qnorm(1-beta/2)*sqrt(var/length(x))

## b ####
size <- .16

(2*qnorm(1-beta/2)/size)^2*var
