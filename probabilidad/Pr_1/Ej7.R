sigma <- 2.7
n <- 200

# a ####
sd_n <- sigma/sqrt(n)
P <- pnorm(mean = 0,sd = sd,q = 0.5)-pnorm(mean = 0, sd = sd_n ,q = -0.5)
P

# b ####
# P(|dist| < .5) < 0.89

dist_lim <- .5
p_min <- .89

N <- ceiling((qnorm((p_min-1)/-2)/(-dist_lim)*sigma)^2)
N
