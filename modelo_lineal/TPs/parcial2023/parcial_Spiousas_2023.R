library(dplyr)
library(readr)
library(ggplot2)

# Ejercicio 1 ####
# Cargo los datos de wine
data_wine <- read_csv("modelo_lineal/parcial/data/wine.csv")
data_wine

## 1.1 ####
mod1 <- lm(data = data_wine,
           Price ~ AGST)
summary(mod1)

# Hay varias formas de calcular sigma2
sigma(mod1)^2 # Lo guarda el modelo ajustado

# Y se puede calcular a mano a partir de la matriz de diseño X
X <- model.matrix(mod1)
n <- nrow(X)
p <- ncol(X)

1/(n-p) * sum((mod1$fitted.values-data_wine$Price)^2)

# O directamente como la suma de los residuos dividido por (n-p)
1/(n-p) * sum((mod1$residuals)^2)
  
# Obviamente todos dan lo mismo

## 1.2 ####
summary(mod1)
# Del summary ya vemos que es significativo, pero lo vamos a hacer a mano

betas <- matrix(mod1$coefficients)
a <- matrix(c(0, 1))
c <- 0

TE_val <- (t(a) %*% betas - c) / sqrt(sigma(mod1)^2 *t(a) %*% solve(t(X)%*%X) %*% a)
TE_val

p_value <- 2*pt(q = abs(TE_val), df = n-p, lower.tail = F)
p_value
# Vemos que el p valor es el mismo

## 1.3 ####
summary(mod1)

## 1.4 ####
data_wine <- data_wine %>%
  mutate(Hum = factor(if_else(WinterRain >= 600, 1, 0)))

## 1.5 ####
mod2 <- lm(data = data_wine,
           Price ~ AGST + Hum)
summary(mod2)

## 1.6 ####
X <- model.matrix(mod2)
n <- nrow(X)
p <- ncol(X)

summary(mod2)
# Del summary ya vemos que Hum1 (beta2) es significativo, pero lo vamos a hacer a mano
betas <- matrix(mod2$coefficients)
a <- matrix(c(0, 0, 1))
c <- 0

TE_val <- (t(a) %*% betas - c) / sqrt(sigma(mod2)^2 *t(a) %*% solve(t(X)%*%X) %*% a)
TE_val

p_value <- 2*pt(q = abs(TE_val), df = n-p, lower.tail = F)
p_value

# En este caso no tiene sentido hacer un test F (de nivel conjunto) poque la variable
# Hum, a pesar de ser discreta, me agrega sólo un parámetro.
# Pero miremos rápidamente la salida de anova de comparar mod1 y mod2 para ver que el 
# p-value es el mismo que el de summary.
anova(mod2, mod1)

## 1.7 ####
data_wine %>% ggplot(aes(x = Year, y = Age)) +
  geom_point() +
  theme_bw()

# La correlación da -1
cor(data_wine$Year, data_wine$Age)

# Pero... ¿Qué pasa si las agregamos?
modprueba <- lm(data = data_wine,
                Price ~ AGST + Hum + Age + Year)
modprueba
# El parámetro de Year aparece como NA porque son TOTALMENTE colineales

## 1.8 ####
mod3 <- lm(data = data_wine,
           Price ~ AGST * Hum)
summary(mod3)

## 1.9 ####
X <- model.matrix(mod3)
n <- nrow(X)
p <- ncol(X)

summary(mod3)
# Del summary ya vemos que AGST:Hum1 (beta12) es significativo, pero lo vamos a hacer a mano
betas <- matrix(mod3$coefficients)
a <- matrix(c(0, 0, 0, 1))
c <- 0

TE_val <- (t(a) %*% betas - c) / sqrt(sigma(mod3)^2 *t(a) %*% solve(t(X)%*%X) %*% a)
TE_val

p_value <- 2*pt(q = abs(TE_val), df = n-p, lower.tail = F)
p_value

# En este caso tampoco tiene sentido hacer un test F (de nivel conjunto) poque la interacción
# agrega sólo un parámetro.
# Pero miremos rápidamente la salida de anova de comparar mod3 y mod2 para ver que el 
# p-value es el mismo que el de summary.
anova(mod3, mod2)

## 1.10 ####
mod4 <- lm(data = data_wine,
           Price ~ AGST + Hum + WinterRain + HarvestRain + Age + FrancePop)
summary(mod4)

# La información está en summary pero lo podemos ver a mano
X <- model.matrix(mod4)
n <- nrow(X)
p <- ncol(X)

betas <- matrix(mod4$coefficients)
a <- matrix(c(0, 0, 0, 0, 0, 1, 0))
c <- 0

TE_val <- (t(a) %*% betas - c) / sqrt(sigma(mod4)^2 *t(a) %*% solve(t(X)%*%X) %*% a)
TE_val

p_value <- 2*pt(q = abs(TE_val), df = n-p, lower.tail = F)
p_value
 
## 1.11 ####
mod5 <- lm(data = data_wine,
           Price ~ AGST + Hum + WinterRain + HarvestRain + Age)
summary(mod5)

# La información está en summary pero lo podemos ver a mano
X <- model.matrix(mod5)
n <- nrow(X)
p <- ncol(X)

betas <- matrix(mod5$coefficients)
a <- matrix(c(0, 0, 0, 0, 0, 1))
c <- 0

TE_val <- (t(a) %*% betas - c) / sqrt(sigma(mod5)^2 *t(a) %*% solve(t(X)%*%X) %*% a)
TE_val

p_value <- 2*pt(q = abs(TE_val), df = n-p, lower.tail = F)
p_value

## 1.12 ####
data_wine %>% ggplot(aes(x = Age, y = FrancePop)) +
  geom_point() +
  theme_bw()
# Podemos ver que FrancePop y Age están MUY correlacionadas
# Es esperable porque Age es una medida del paso del tiempo y la población mundial
# crecía monotonamente (al menos hasta los 90s)
cor(data_wine$Age, data_wine$FrancePop)

## 1.13 ####
A <- matrix(c(0, 0, 
              0, 0, 
              0, 0, 
              0, 0, 
              0, 0, 
              1, 0,
              0, 1), nrow = 2)
A
C <- matrix(c(0, 0), nrow = 2)
C

X <- model.matrix(mod4)
n <- nrow(X)
p <- ncol(X)

betas <- matrix(mod4$coefficients)

# Calculo el F
F_val <- t(A %*% betas) %*% solve(A %*% solve(t(X) %*% X) %*% t(A)) %*% (A %*% betas) / (nrow(A)*summary(mod4)$sigma^2)
F_val

# Y ahora el p-value
pf(df1 = nrow(A), df2 = nrow(X)-ncol(X), q = as.numeric(F_val), lower.tail = FALSE)

# Ejercicio 2 ####

## 2.1 ####
set.seed(2208)
n <- 60

x1 <- round(rnorm(n = n, mean = 10, sd = 5), digits = 2)
x2 <- round(rnorm(n = n, mean = 10, sd = 5), digits = 2)

Y <- 2 - 3*x1 + 2*x2 - 10*x1*x2 + rnorm(n = n)

data_simul <- tibble(x1, x2, Y)

### 2.1.a ####
mod_simul1 <- lm(data = data_simul,
                Y ~ x1 * x2)
summary(mod_simul1)

beta1_est <- mod_simul1$coefficients[2]
beta1_est

### 2.1.b ####
CI_1 <- confint(mod_simul1, level = .9)[2,]
CI_1

# También lo puedo hacer a mano
betas <- matrix(mod_simul1$coefficients)
X <- model.matrix(mod_simul1)

n <- nrow(model.matrix(mod_simul1))
p <- ncol(model.matrix(mod_simul1))

alpha <- .1
# Intervalos de confianza para el beta_1
a <- matrix(c(0, 1, 0, 0))
t(a) %*%  betas # Valor predicho

t(a) %*%  betas + qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(mod_simul1)  * sqrt(t(a) %*% Matrix::solve(t(X)%*%X) %*% a)
t(a) %*%  betas - qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(mod_simul1)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)
# Da lo mismo

### 2.1.c ####
# El valor verdadero de beta1 es -3
beta1 <- -3
between(beta1, CI_1[1], CI_1[2])
# El valor verdadero de beta1 está contenido en el intervalo calculado en la simulación

### 2.1.d ####
mod_simul2 <- lm(data = data_simul,
                 Y ~ x1 + x2)
summary(mod_simul2)

alpha1_est <- mod_simul2$coefficients[2]
alpha1_est

### 2.1.e ####
# Sólo lo voy a hacer con confint
CI_2 <- confint(mod_simul2, level = .9)[2,]
CI_2

### 2.1.f ####
# El valor verdadero de beta1 es -3
beta1 <- -3
between(beta1, CI_2[1], CI_2[2])
# El valor verdadero de beta1 NO está contenido en el intervalo calculado en la simulación
# que no tiene en cuenta la interacción

## 2.2 ####
Nrep <- 1000

beta1 <- -3
beta1_hat <- rep(NA, Nrep)
alpha1_hat <- rep(NA, Nrep)
cubrimiento_beta1 <- rep(NA, Nrep)
cubrimiento_alpha1 <- rep(NA, Nrep)
set.seed(2208)
for (i in 1:Nrep) {
  Y <- 2 - 3*x1 + 2*x2 - 10*x1*x2 + rnorm(n = n)  
  data_simul <- tibble(x1, x2, Y)
  
  # Modelo con ineracción
  mod_simul1 <- lm(data = data_simul,
                   Y ~ x1 * x2)
  beta1_hat[i] <- mod_simul1$coefficients[2]
  CI_1 <- confint(mod_simul1, level = .9)[2,]
  cubrimiento_beta1[i] <- between(beta1, CI_1[1], CI_1[2])
  
  # Modelo sin interacción
  mod_simul2 <- lm(data = data_simul,
                   Y ~ x1 + x2)
  alpha1_hat[i] <- mod_simul2$coefficients[2]
  CI_2 <- confint(mod_simul2, level = .9)[2,]
  cubrimiento_alpha1[i] <- between(beta1, CI_2[1], CI_2[2])
}

simuls <- tibble(alpha1_hat, beta1_hat, cubrimiento_alpha1, cubrimiento_beta1)

### 2.2.a ####
simuls %>% ggplot(aes(x = beta1_hat, y = after_stat(..density..))) +
  geom_histogram(alpha = .5) +
  theme_bw()

mean(simuls$beta1_hat)
# La media pareciera estar cerca del valor verdadero, por lo tanto, podemos afirmar
# que beta1_hat parece un estimador insesgado de beta1

### 2.2.b ####
mean(simuls$cubrimiento_beta1) * 100
# El cubrimiento del intervalo de confianza de beta1_hat, el parámetro 1 del modelo
# con interacción, contiene al verdadero valor en el 90% de los casos. Esto resulta 
# esperable ya que el mismo es un estimador insesgado de beta0 y a partir de su 
# distribución se calcula el intervalo de confianza por confint (pivot de dist t, 
# porque usa la sd estimada)

### 2.2.c ####
simuls %>% ggplot(aes(x = alpha1_hat, y = after_stat(..density..))) +
  geom_histogram(alpha = .5) +
  theme_bw()

mean(simuls$alpha1_hat)
# El valor verdadero de beta1 está MUY lejos de la media de alpha, y viendo el 
# histograma ni siquiera aparece cerca. Por lo tanto, alpha_1 no pareciera ser
# un estimador insesgado de beta1.

### 2.2.d ####
mean(simuls$cubrimiento_alpha1) * 100
# El intervalo de confianza de alpha1_hat NUNCA contiene al verdadero valor de beta1.
# En el histograma ya se veía que la estimaciones puntuales de alpha1 estaban MUY
# lejos del verdadero valor de beta1, por lo tanto también es esperable.

### 2.2.e ####
# Les estimaciones del error estándar
sd(simuls$beta1_hat)
sd(simuls$alpha1_hat)
