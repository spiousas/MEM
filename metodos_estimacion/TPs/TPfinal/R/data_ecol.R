library(ash)
library(MASS)

x <- rnorm(1000, 5, 4)
#x <- runif(1000, 0, 10)
#y <- runif(1000, 0, 10)
x_trim <- x[x>0 & x<10]
y <- runif(length(x_trim), 0, 10)
plot(y~x_trim)

bins <- bin2(cbind(x,y), nbin = c(10,10))

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
