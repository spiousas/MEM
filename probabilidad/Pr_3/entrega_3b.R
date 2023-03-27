# a ####
n <- 4
Xn_obs <- 65.43
sigma <- 10
mu <- 70

2*pnorm(-abs(Xn_obs - mu) / (sigma/sqrt(n)))

# b ####
n <- 40

2*pnorm(-abs(Xn_obs - mu) / (sigma/sqrt(n)))
