library(ash)
library(MASS)

x <- rnorm(1000, 5, 4)
x <- matrix( rnorm(300, sd = .1), 100 , 2)   
#x <- runif(500, 0, 10)
#y <- runif(500, 0, 10)
x_trim <- x[x>0 & x<10]
y <- runif(length(x_trim), 0, 10)
plot(y~x_trim)
#plot(y~x)
plot(x)

bins <- bin2(x, nbin = c(10,10))
bins
est_mv <- fitdistr(as.vector(bins$nc), densfun = "poisson")
x <- 1:max(as.vector(bins$nc))
dens <- dpois(x = x, lambda = est_mv$estimate[1])
hist(bins$nc, freq = F)
lines(x, dens)


est_mv <- fitdistr(as.vector(bins$nc), densfun = "negative binomial")
mean(as.vector(bins$nc))
var(as.vector(bins$nc))
est_mv$estimate
10+10^2/15
x <- 1:max(as.vector(bins$nc))
dens <- dnbinom(x = x, size = est_mv$estimate[1], mu = est_mv$estimate[2])
lines(x, dens)

