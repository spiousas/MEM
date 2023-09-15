pacman::p_load(tidyverse, here, MASS)
data("Boston")

# 1 ####
mod1 <- lm(data = Boston,
           medv ~ zn + indus)
summary(mod1)

sigma(mod1)^2
X <- model.matrix(mod1)
n <- nrow(X)
p <- ncol(X)

S2 <- 1/(n-p) * sum((mod1$fitted.values-Boston$medv)^2)
S2

1/(n-p) * sum((mod1$residuals)^2)

# 2 ####
new_data <- tibble(zn = 1, indus = 3)
predict(mod1, newdata = new_data)
predict(mod1, newdata = new_data, interval = "confidence", level = .9)

# A mano
betas <- matrix(mod1$coefficients)
xh <- matrix(c(1, 1, 3))
t(xh) %*%  betas # Valor predicho

# Intervalos de confianza
alpha <- .1
t(xh) %*%  betas + qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(mod1)  * sqrt(t(xh) %*% solve(t(X)%*%X) %*% xh)
t(xh) %*%  betas - qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(mod1)  * sqrt(t(xh) %*% solve(t(X)%*%X) %*% xh)

# 3 ####
a <- matrix(c(0, 10, 1))
c <- 0

TE_val <- (t(a) %*% betas - c) / sqrt(sigma(mod1)^2 *t(a) %*% solve(t(X)%*%X) %*% a)
TE_val

p_value <- 2*pt(q = abs(TE_val), df = n-p, lower.tail = F)
p_value

# 4 ####
Boston <- Boston %>%
  mutate(radc = case_when(
    rad <= 3 ~ 2,
    (rad > 3) & (rad <= 8) ~ 1,
    rad > 8 ~ 0
  ),
  radc = factor(radc))
Boston

# 5 ####
mod2 <- lm(data = Boston,
           medv ~ zn + indus + radc)
summary(mod2)

# 6 ####
anova(mod2, mod1)

A <- matrix(c(0, 0, 
              0, 0, 
              0, 0, 
              1, 0, 
              0, 1), nrow = 2)
A
C <- matrix(c(0, 0), nrow = 2)
C

# Los parámetros estimados
betas <- matrix(mod2$coefficients)

# La matriz del modelo
X <- model.matrix(mod2)
X

# Calculo el F
F_val <- t(A %*% betas - C) %*% solve(A %*% solve(t(X) %*% X) %*% t(A)) %*% (A %*% betas - C) / (nrow(A)*summary(mod2)$sigma^2)
F_val

pf(df1 = nrow(A), df2 = nrow(X)-ncol(X), q = as.numeric(F_val), lower.tail = FALSE)

# 7 ####
mod3 <- lm(data = Boston,
           medv ~ zn + indus + radc + crim)
summary(mod3)

# 8 ####
anova(lm(data = Boston, medv ~ zn + indus + crim), mod3)

A <- matrix(c(0, 0, 
              0, 0, 
              0, 0, 
              1, 0, 
              0, 1,
              0, 0), nrow = 2)
A
C <- matrix(c(0, 0), nrow = 2)
C

# Los parámetros estimados
betas <- matrix(mod3$coefficients)

# La matriz del modelo
X <- model.matrix(mod3)
X

# Calculo el F
F_val <- t(A %*% betas - C) %*% solve(A %*% solve(t(X) %*% X) %*% t(A)) %*% (A %*% betas - C) / (nrow(A)*summary(mod3)$sigma^2)
F_val

pf(df1 = nrow(A), df2 = nrow(X)-ncol(X), q = as.numeric(F_val), lower.tail = FALSE)

# 10 ####
interaction.plot(Boston$radc, Boston$chas, response = Boston$medv)

# 11 ####
mod4 <- lm(data = Boston,
           medv ~ zn + indus + radc * chas)
summary(mod4)

# 12 ####
mod4 <- lm(data = Boston,
           medv ~ zn + indus + radc * chas)
summary(mod4)

anova(lm(data = Boston, medv ~ zn + indus + radc + chas), mod4)

A <- matrix(c(0, 0, 
              0, 0, 
              0, 0, 
              0, 0, 
              0, 0,
              0, 0,
              1, 0,
              0, 1), nrow = 2)
A
C <- matrix(c(0, 0), nrow = 2)
C

# Los parámetros estimados
betas <- matrix(mod4$coefficients)

# La matriz del modelo
X <- model.matrix(mod4)
X

# Calculo el F
F_val <- t(A %*% betas - C) %*% solve(A %*% solve(t(X) %*% X) %*% t(A)) %*% (A %*% betas - C) / (nrow(A)*summary(mod4)$sigma^2)
F_val

pf(df1 = nrow(A), df2 = nrow(X)-ncol(X), q = as.numeric(F_val), lower.tail = FALSE)
