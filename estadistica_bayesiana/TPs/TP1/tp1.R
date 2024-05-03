# Ejercicio 6 ####
P <- tibble(lambda = seq(0.5,3,0.5),
            p = c(0.1, 0.2, 0.3, 0.2, 0.15, 0.05))

y <- 12
t <- 6

den <- sum((P$lambda * t)^y * exp(-P$lambda * t)/factorial(y) * P$p)
den

P <- P %>%
  mutate(post =  ((lambda * t)^y * exp(-lambda * t)/factorial(y) * p)/den)

esperanza <- sum(P$post * P$lambda)
esperanza
