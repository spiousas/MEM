datos_entrega <- scan()

## a ####
lambda_est <- 1/mean(datos_entrega)
1/lambda_est

## b ####
lim <- 18
1-exp(-lambda_est*lim)

## c ####
p <- 0.66
-1/lambda_est * log(p)

## d ####
lambda <- 1/27
lim <- 18
lim^2 * exp(-2*lim*lambda) * (lambda)^2 / ( (1-exp(-lim*lambda)) * exp(-lim*lambda) )
