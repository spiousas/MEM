library(tidyverse)
library(here)

# 1 ####
set.seed(1)
data <- runif(n = 25, min = 0, max = 1)

# 2 ####
pdf(file = here("aprendizaje_estadistico/Pr_4/figs/Ej2.pdf"), 
    width = 8, 
    height = 8)

par(mfrow=c(1,1))
plot(ecdf(data),
     main="Ej. 2 - Unif[0,1], n=25, seed=1")
x <- seq(0,1,0.01)
lines(x, x, col = "red")

dev.off()

# 3 ####
x <- seq(0,1,0.01)
N <- 25

pdf(file = here("aprendizaje_estadistico/Pr_4/figs/Ej3.pdf"),  
    width = 8,
    height = 8)

par(mfrow=c(2,2))

set.seed(1)
data <- runif(n = N, min = 0, max = 1)
plot(ecdf(data),
     main="Ej. 3 - Unif[0,1], n=25, seed=1")
lines(x, x, col = "red")

set.seed(2)
data <- runif(n = N, min = 0, max = 1)
plot(ecdf(data),
     main="Ej. 3 - Unif[0,1], n=25, seed=2")
lines(x, x, col = "red")

set.seed(3)
data <- runif(n = N, min = 0, max = 1)
plot(ecdf(data),
     main="Ej. 3 - Unif[0,1], n=25, seed=3")
lines(x, x, col = "red")

set.seed(4)
data <- runif(n = N, min = 0, max = 1)
plot(ecdf(data),
     main="Ej. 3 - Unif[0,1], n=25, seed=4")
lines(x, x, col = "red")

dev.off()

# 4 ####
set.seed(123)
x <- seq(0,1,0.01)
N <- 1000

pdf(file = here("aprendizaje_estadistico/Pr_4/figs/Ej4.pdf"),  
    width = 8,
    height = 8)

par(mfrow=c(2,2))

set.seed(1)
data <- runif(n = N, min = 0, max = 1)
plot(ecdf(data),
     main="Ej. 4 - Unif[0,1], n=1000, seed=1")
lines(x, x, col = "red")

set.seed(2)
data <- runif(n = N, min = 0, max = 1)
plot(ecdf(data),
     main="Ej. 4 - Unif[0,1], n=1000, seed=2")
lines(x, x, col = "red")

set.seed(3)
data <- runif(n = N, min = 0, max = 1)
plot(ecdf(data),
     main="Ej. 4 - Unif[0,1], n=1000, seed=3")
lines(x, x, col = "red")

set.seed(4)
data <- runif(n = N, min = 0, max = 1)
plot(ecdf(data),
     main="Ej. 4 - Unif[0,1], n=1000, seed=4")
lines(x, x, col = "red")

dev.off()

# 5 ####
set.seed(1)
data <- rexp(n = 25, rate = 1)

pdf(file = here("aprendizaje_estadistico/Pr_4/figs/Ej5_1.pdf"),  
    width = 8,
    height = 8)

par(mfrow=c(1,1))
plot(ecdf(data),
     main="Ej. 5 - Exp(1), n=25, seed=1")
x <- seq(0,5,0.01)
lines(x, 1-exp(-x), col = "blue")

dev.off()

# n=25 ####
x <- seq(0,6,0.01)
N <- 25

pdf(file = here("aprendizaje_estadistico/Pr_4/figs/Ej5_2.pdf"),  
    width = 8,
    height = 8)

par(mfrow=c(2,2))

set.seed(1)
data <- rexp(n = N, rate = 1)
plot(ecdf(data),
     main="Ej. 5 - Exp(1), n=25, seed=1",
     xlim = c(0,6))
lines(x, 1-exp(-x), col = "blue")

set.seed(2)
data <- rexp(n = N, rate = 1)
plot(ecdf(data),
     main="Ej. 5 - Exp(1), n=25, seed=2",
     xlim = c(0,6))
lines(x, 1-exp(-x), col = "blue")

set.seed(3)
data <- rexp(n = N, rate = 1)
plot(ecdf(data),
     main="Ej. 5 - Exp(1), n=25, seed=3",
     xlim = c(0,6))
lines(x, 1-exp(-x), col = "blue")

set.seed(4)
data <- rexp(n = N, rate = 1)
plot(ecdf(data),
     main="Ej. 5 - Exp(1), n=25, seed=4",
     xlim = c(0,6))
lines(x, 1-exp(-x), col = "blue")

dev.off()

# n=1000 ####
x <- seq(0,8,0.01)
N <- 1000

pdf(file = here("aprendizaje_estadistico/Pr_4/figs/Ej5_3.pdf"),  
    width = 8,
    height = 8)

par(mfrow=c(2,2))

set.seed(1)
data <- rexp(n = N, rate = 1)
plot(ecdf(data),
     main="Ej. 5 - Exp(1), n=1000, seed=1",
     xlim = c(0,8))
lines(x, 1-exp(-x), col = "blue")

set.seed(2)
data <- rexp(n = N, rate = 1)
plot(ecdf(data),
     main="Ej. 5 - Exp(1), n=1000, seed=2",
     xlim = c(0,8))
lines(x, 1-exp(-x), col = "blue")

set.seed(3)
data <- rexp(n = N, rate = 1)
plot(ecdf(data),
     main="Ej. 5 - Exp(1), n=1000, seed=3",
     xlim = c(0,8))
lines(x, 1-exp(-x), col = "blue")

set.seed(4)
data <- rexp(n = N, rate = 1)
plot(ecdf(data),
     main="Ej. 5 - Exp(1), n=1000, seed=4",
     xlim = c(0,8))
lines(x, 1-exp(-x), col = "blue")

dev.off()

# 6 ####
x <- seq(0,1,0.01)

pdf(file = here("aprendizaje_estadistico/Pr_4/figs/Ej6.pdf"),  
    width = 8,
    height = 8)

par(mfrow=c(1,1))
plot(x, x, type='n',
     main="Ej. 6 - Unif[0,1] (azul) y Exp(1) (rojo)",
     xlab="x",
     ylab="Fn(x)")
lines(x,x, col = "red")
lines(x, 1-exp(-x), col = "blue")

dev.off()