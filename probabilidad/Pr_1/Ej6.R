library(tidyverse)
library(magrittr)

rm(list = ls())

# Guia ####
p1 <- 5
p2 <- 15

## a ####
minRec <- 23
acum <- 0
for (i in 0:minRec-1) {
  acum <- acum + dpois(lambda = p1+p2, x = i)
}
1 - acum
1 - ppois(lambda = p1+p2, q = minRec-1)

## b ####
total_calls <- 18
factura_calls <- 8

# P(X=8)
dpois(lambda = p1, x = factura_calls)
# P(Y=10)
dpois(lambda = p2, x = total_calls - factura_calls)
# P(Z=18)
dpois(lambda = p1+p2, x = total_calls)
# P(X=8|Z=18)
(dpois(lambda = p1, x = factura_calls) * dpois(lambda = p2, x = total_calls - factura_calls)) / dpois(lambda = p1+p2, x = total_calls)

# Entregable ####
p1 <- 4
p2 <- 14

## a ####
minRec <- 22
acum <- 0
for (i in 0:minRec-1) {
  acum <- acum + dpois(lambda = p1+p2, x = i)
}
1 - acum
1 - ppois(lambda = p1+p2, q = minRec-1)

## b ####
total_calls <- 17
factura_calls <- 4

# P(X=8)
dpois(lambda = p1, x = factura_calls)
# P(Y=10)
dpois(lambda = p2, x = total_calls - factura_calls)
# P(Z=18)
dpois(lambda = p1+p2, x = total_calls)
# P(X=8|Z=18)
(dpois(lambda = p1, x = factura_calls) * dpois(lambda = p2, x = total_calls - factura_calls)) / dpois(lambda = p1+p2, x = total_calls)
