n <- 163
curados <- 31
phat <- curados/n

nivel <- 0.99
alfa <- 1-nivel

lmax <- 0.06

# a ####
phat - qnorm(1-alfa/2) * sqrt(phat*(1-phat)/n)

# b ####
phat + qnorm(1-alfa/2) * sqrt(phat*(1-phat)/n) 

# c ####
ceiling((qnorm(1-alfa/2) / lmax)^2)

