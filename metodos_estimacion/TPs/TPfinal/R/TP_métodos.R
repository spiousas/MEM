library(here)
library(MASS)

dias <- read.table(here("metodos_estimacion/TPs/TPfinal/R/data/dias.txt"))
datos <- as.vector(t(dias))

mean(datos)
var(datos)

emv_NB <- fitdistr(datos, densfun = "negative binomial")
emv_Poi <- fitdistr(datos, densfun = "poisson")

hist(datos,freq = FALSE)

grilla <- seq(0,40,1)

dens_mv_BN <- dnbinom(grilla, size=emv_NB$estimate[1], mu=emv_NB$estimate[2])
dens_mv_Poi <- dpois(grilla, lambda = emv_Poi$estimate[1])

lines(grilla,dens_mv_BN)
lines(grilla,dens_mv_Poi,col="red")


