pacman::p_load(tidyverse, here, janitor, matlib)

# Ejercicio 1 ####
## a ####
n <- 10
beta0 <- 5
beta1 <- 1
beta2 <- 3 

X1 <- runif(n,0,10)
X2 <- runif(n,0,10)

e <- rnorm(n)
Y <- beta0 + beta1*X1 + beta2*X2 + e
Y

data <- tibble(X1, X2, Y)
data
model <- lm(data = data, Y ~ X1 + X2)
summary(model)

## b ####
Nrep <- 1000
betas1 <- rep(NA, Nrep)
betas2 <- rep(NA, Nrep)
sd_beta1 <- rep(NA, Nrep)
sd_beta2 <- rep(NA, Nrep)

set.seed(1234)
for (i in 1:Nrep) {
  e <- rnorm(n)
  Y <- beta0 + beta1*X1 + beta2*X2 + e
  
  data <- tibble(X1, X2, Y)
  model <- lm(data = data, Y ~ X1 + X2)
  
  betas1[i] <- model$coefficients[2]
  betas2[i] <- model$coefficients[3]
  sd_beta1[i] <- summary(model)$coefficients[2,2]
  sd_beta2[i] <- summary(model)$coefficients[3,2]
  
}

## c ####
hist(betas1)

## d ####
# Los betas estimados vienen de una normal multivariada de medias betas y 
# matriz de covarianza dada por sigma^2*(t(X) %*% X)^(-1), donde sigma^2 es la 
# varianza del error (1 en nuestro caso).

model <- lm(data = data, Y ~ X1 + X2)
X <- model.matrix(model)
mu <- matrix(c(beta1, beta2), nrow = 2)
mu
var <- inv(t(X) %*% X)
var[2,2]
var[3,3]

## e ####
T <- (betas1 - beta1)/sd_beta1

## f, g y h ####
tibble(T) %>% 
  ggplot() +
  geom_histogram(aes(x = T, y = after_stat(..density..)), alpha = .5) + 
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = sqrt(1)), color = "red") +
  stat_function(fun = dt, n = 101, args = list(df = n-2), color = "blue") +
  theme_bw()

## i y j ####
cubrimiento_beta_1 <- rep(NA, Nrep)
cubrimiento_beta_2 <- rep(NA, Nrep)

set.seed(1234)
for (i in 1:Nrep) {
  e <- rnorm(n)
  Y <- beta0 + beta1*X1 + beta2*X2 + e
  
  data <- tibble(X1, X2, Y)
  model <- lm(data = data, Y ~ X1 + X2)
  
  CI <- confint(model, level = .9)
  
  cubrimiento_beta_1[i] <- between(beta1, CI[2,1], CI[2,2])
  cubrimiento_beta_2[i] <- between(beta2, CI[3,1], CI[3,2])
}

sum(cubrimiento_beta_1)/Nrep
sum(cubrimiento_beta_2)/Nrep

## k ####
sum(cubrimiento_beta_1 & cubrimiento_beta_2)/Nrep

# Ejercicio 2 ####
n <- 150
X1 <- runif(n,0,10)
X2 <- runif(n,0,10)

## La simulación ####
Nrep <- 1000
betas1 <- rep(NA, Nrep)
betas2 <- rep(NA, Nrep)
sd_beta1 <- rep(NA, Nrep)
sd_beta2 <- rep(NA, Nrep)
cubrimiento_beta_1 <- rep(NA, Nrep)
cubrimiento_beta_2 <- rep(NA, Nrep)

set.seed(1234)
for (i in 1:Nrep) {
  e <- rnorm(n)
  Y <- beta0 + beta1*X1 + beta2*X2 + e
  
  data <- tibble(X1, X2, Y)
  model <- lm(data = data, Y ~ X1 + X2)
  
  betas1[i] <- model$coefficients[2]
  betas2[i] <- model$coefficients[3]
  sd_beta1[i] <- summary(model)$coefficients[2,2]
  sd_beta2[i] <- summary(model)$coefficients[3,2]
  
  CI <- confint(model, level = .9)
  
  cubrimiento_beta_1[i] <- between(beta1, CI[2,1], CI[2,2])
  cubrimiento_beta_2[i] <- between(beta2, CI[3,1], CI[3,2])
}

hist(betas1)

## Estadístico T ####
T <- (betas1 - beta1)/sd_beta1

tibble(T) %>% 
  ggplot() +
  geom_histogram(aes(x = T, y = after_stat(..density..)), alpha = .5) + 
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = sqrt(1)), color = "red") +
  stat_function(fun = dt, n = 101, args = list(df = n-2), color = "blue") +
  theme_bw()

## CIs y cubrimiento ####
model <- lm(data = data, Y ~ X1 + X2)
confint(model, level = .9)
# Los intervalos de confianza son mucho más finos, es decir, la estimación de 
# beta_1 y beta_2 es más precisa.

sum(cubrimiento_beta_1)/Nrep
sum(cubrimiento_beta_2)/Nrep

sum(cubrimiento_beta_1 & cubrimiento_beta_2)/Nrep

# Ejercicio 3 ####
## Con n=10 ####
n <- 10
X1 <- runif(n,0,10)
X2 <- runif(n,0,10)

### La simulación ####
Nrep <- 1000
betas1 <- rep(NA, Nrep)
betas2 <- rep(NA, Nrep)
sd_beta1 <- rep(NA, Nrep)
sd_beta2 <- rep(NA, Nrep)
cubrimiento_beta_1 <- rep(NA, Nrep)
cubrimiento_beta_2 <- rep(NA, Nrep)

set.seed(78454)
for (i in 1:Nrep) {
  e <- rnorm(n)
  Y <- beta0 + beta1*X1 + beta2*X2 + e
  
  data <- tibble(X1, X2, Y)
  model <- lm(data = data, Y ~ X1 + X2)
  
  betas1[i] <- model$coefficients[2]
  betas2[i] <- model$coefficients[3]
  sd_beta1[i] <- summary(model)$coefficients[2,2]
  sd_beta2[i] <- summary(model)$coefficients[3,2]
  
  CI <- confint(model, level = .9)
  
  cubrimiento_beta_1[i] <- between(beta1, CI[2,1], CI[2,2])
  cubrimiento_beta_2[i] <- between(beta2, CI[3,1], CI[3,2])
}

hist(betas1)

### Estadístico T ####
T <- (betas1 - beta1)/sd_beta1

tibble(T) %>% 
  ggplot() +
  geom_histogram(aes(x = T, y = after_stat(..density..)), alpha = .5) + 
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = sqrt(1)), color = "red") +
  stat_function(fun = dt, n = 101, args = list(df = n-2), color = "blue") +
  theme_bw()

### CIs y cubrimiento ####
sum(cubrimiento_beta_1)/Nrep
sum(cubrimiento_beta_2)/Nrep

sum(cubrimiento_beta_1 & cubrimiento_beta_2)/Nrep

## Con n=150 ####
n <- 150
X1 <- runif(n,0,10)
X2 <- runif(n,0,10)

### La simulación ####
Nrep <- 1000
betas1 <- rep(NA, Nrep)
betas2 <- rep(NA, Nrep)
sd_beta1 <- rep(NA, Nrep)
sd_beta2 <- rep(NA, Nrep)
cubrimiento_beta_1 <- rep(NA, Nrep)
cubrimiento_beta_2 <- rep(NA, Nrep)

set.seed(1234)
for (i in 1:Nrep) {
  e <- rnorm(n)
  Y <- beta0 + beta1*X1 + beta2*X2 + e
  
  data <- tibble(X1, X2, Y)
  model <- lm(data = data, Y ~ X1 + X2)
  
  betas1[i] <- model$coefficients[2]
  betas2[i] <- model$coefficients[3]
  sd_beta1[i] <- summary(model)$coefficients[2,2]
  sd_beta2[i] <- summary(model)$coefficients[3,2]
  
  CI <- confint(model, level = .9)
  
  cubrimiento_beta_1[i] <- between(beta1, CI[2,1], CI[2,2])
  cubrimiento_beta_2[i] <- between(beta2, CI[3,1], CI[3,2])
}

hist(betas1)

### Estadístico T ####
T <- (betas1 - beta1)/sd_beta1

tibble(T) %>% 
  ggplot() +
  geom_histogram(aes(x = T, y = after_stat(..density..)), alpha = .5) + 
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = sqrt(1)), color = "red") +
  stat_function(fun = dt, n = 101, args = list(df = n-2), color = "blue") +
  theme_bw()

### CIs y cubrimiento ####
sum(cubrimiento_beta_1)/Nrep
sum(cubrimiento_beta_2)/Nrep

sum(cubrimiento_beta_1 & cubrimiento_beta_2)/Nrep

## Conclusiones ####
# En ambos casos el cubrimiento es muy bueno, aunque con n=10 la distribución de beta_1
# pareciera estar sesgada.

# Ejercicio 4 ####
paralel <- read_csv(here("modelo_lineal/TPs/TP4/data/paralel.csv"))
names(paralel)
paralel

## a ####
n1 <- length(paralel$x1)
n2 <- length(paralel$x2)
y <- c(paralel$y1,paralel$y2)
# Fabricamos la martriz de diseño a mano
X <- matrix(0,n1+n2,2)
X[1:n1,1] <- rep(1,n1)
X[(n1+1):(n1+n2),2] <- rep(1,n2)

ajuste <- lm(y~X-1) ## No queremos ajustar con intercept, por eso el -1
summary(ajuste)
p <- dim(X)[2]
n <- length(y)
s2 <- sum(ajuste$residuals^2)/(n-p)
# s2 <- summary(ajuste)$sigma^2 # Da lo mismo así

# Mis hipótesis son
# H0: mu0 = mu1
# vs.
# H1: mu0 != mu1
# H0 también se puede escribir como mu0 - mu1 = 0, por eso escribo al vector a como:
a <- c(1,-1)
# Construimos el estadistico del test en este caso (n este caso c=0)
TE <- (t(a)%*%(ajuste$coefficients))/sqrt(s2*t(a)%*%solve(t(X)%*%X)%*%a) ## ver tema vector, matriz, dimension

pvalor <- 2*pt(TE, df=n-p,lower.tail = FALSE)
pvalor

## Funcion que sirve para testear H_0: a^T beta = c versus H_0: a^T beta != c
test.cl.beta <- function(X,y,a,c)
{
  ajuste <- lm(y~X-1)
  summary(ajuste)
  p <- dim(X)[2]
  n <- length(y)
  s2 <- sum(ajuste$residuals^2)/(n-p)
  
  TE <- (t(a)%*%(ajuste$coefficients)-c)/sqrt(s2*t(a)%*%solve(t(X)%*%X)%*%a)
  
  pvalor <- 2*pt(abs(TE), df=n-p,lower.tail = FALSE)
  pvalor
  
}
test.cl.beta(X,y,c(1,-1),c=0)
# Esto también se puede hacer con el t.test de toda la vida.
t.test(paralel$y1,paralel$y2,var.equal = TRUE)

## Alternativa con intercept
# Tiene la ventaja de que el parámetro (que es la diferencia entre las medias) es
# directamente el test que estamos buscando
n1 <- length(paralel$x1)
n2 <- length(paralel$x2)
y <- c(paralel$y1,paralel$y2)
#fabricamos la matriz de disenio a mano
X <- matrix(0,n1+n2,1)
X[1:n1,1] <- rep(0,n1)
X[(n1+1):(n1+n2),1] <- rep(1,n2)
ajuste <- lm(y~X) ## No queremos ajustar con intercept ?que pasa si ajusto con intercept?
summary(ajuste)

# Ejercicio 5 ####
data_paralel <- read_delim(here("modelo_lineal/TPs/TP4/data/paralel.csv"))

# La matriz X queda definida de esta forma (con 0s y 1s metidos en el medio)
X <- matrix(c(rep(1, 20), rep(0,20), data_paralel$x1, rep(0,20),
              rep(0, 20), rep(1,20), rep(0,20), data_paralel$x2), 
            ncol = 4)
# La matriz Y es simplemente concatenar las Ys
Y <- matrix(c(data_paralel$y1, data_paralel$y2), 
           ncol = 1)

# Calculo los betas y la varianza de los sigmas estimada a partir de la matriz
# de diseño y las Ys
betas <- inv(t(X) %*% X) %*% t(X) %*% Y
sigma2 <- 1/(n-p) * t(Y-X %*% betas) %*% (Y-X %*% betas)

# Para los grados de libertad
n <- nrow(Y)
p <- ncol(X)

# Las matrices del test
a <- matrix(c(0, 1, 0 , -1), ncol = 1)
c <- 0

# El estadístico
TE <- (t(a)%*%(betas)-c)/sqrt(sigma2*t(a)%*%solve(t(X)%*%X)%*%a)
TE

# El p-valor
p_value <- 2*pt(q = TE, df = n-p, lower.tail = F)
p_value 

# Ejercicio 6 ####

data_salary <- read_delim(here("modelo_lineal/TPs/TP4/data/salary.txt")) %>%
  clean_names() %>%
  mutate(sex = if_else(sex==0, "Hombre", "Mujer"),
         rank = if_else(rank==1, "Assistant Professor", if_else(rank==2, "Associate Professor", "Full Profesor")))

## a ####
data_salary %>%
  ggplot(aes(x = year,
             y = salary,
             color = sex)) +
  geom_point() + 
  theme_bw()
    
data_salary %>%
  ggplot(aes(x = y_sdeg,
             y = salary,
             color = sex)) +
  geom_point() + 
  theme_bw()

data_salary %>%
  ggplot(aes(x = sex,
             y = salary,
             color = sex)) +
  geom_boxplot() +
  geom_jitter() + 
  theme_bw()

data_salary %>%
  ggplot(aes(x = rank,
             y = salary,
             color = sex)) +
  geom_boxplot() +
  theme_bw()

## a ####
x <- data_salary$salary[data_salary$sex=="Hombre"]
y <- data_salary$salary[data_salary$sex=="Mujer"]
t.test(x,y)

# Ganan más los hombres pero la diferencia no es significativa.

## c.i ####
model_salary <- lm(data = data_salary,
                   salary ~ sex + year)
summary(model_salary)
confint(model_salary)

# Intervalo de confianza para sigma
sigma <- sigma(model_salary)
sigma

n <- nrow(data_salary)
k <- 2 # Dos predictores
alpha <- 0.05

lower <- (n-(k+1))*sigma^2/qchisq(alpha/2, df = n-(k+1), lower.tail = FALSE)
upper <- (n-(k+1))*sigma^2/qchisq(1-alpha/2, df = n-(k+1), lower.tail = FALSE)

confint_sigma <- round(sqrt(c(lower, upper)), 4)
names(confint_sigma) <- c("lower", "upper")
confint_sigma

## c.ii ####
# La hipótesis nula es que en parámetro (el que corresponde) es igual a cero y la 
# alternativa es que es diferente de cero

## c.iii ####
colormap <- c("#E69F00", "#56B4E9")
data_salary %>%
  ggplot(aes(x = year,
             y = salary,
             color = sex)) +
  geom_point() + 
  geom_abline(intercept = model_salary$coefficients[1], slope = model_salary$coefficients[3], color = colormap[1], linewidth = 1) +
  geom_abline(intercept = model_salary$coefficients[1] + model_salary$coefficients[2], slope = model_salary$coefficients[3], color = colormap[2], linewidth = 1) +
  scale_color_manual(values =colormap) +
  theme_bw() +
  theme(legend.position = "top")

# Observo que la reta para mujeres está ligeramente por encima de la recta para varones

## c.iv ####
model_salary_mujer <- lm(data = data_salary %>% filter(sex=="Mujer"),
                   salary ~ year)
summary(model_salary_mujer)

model_salary_hombre <- lm(data = data_salary %>% filter(sex=="Hombre"),
                   salary ~ year)
summary(model_salary_hombre)

colormap <- c("#E69F00", "#56B4E9")
data_salary %>%
  ggplot(aes(x = year,
             y = salary,
             color = sex)) +
  geom_point() + 
  geom_abline(intercept = model_salary_hombre$coefficients[1], slope = model_salary_hombre$coefficients[2], color = colormap[1], linewidth = 1) +
  geom_abline(intercept = model_salary_mujer$coefficients[1], slope = model_salary_mujer$coefficients[2], color = colormap[2], linewidth = 1) +
  scale_color_manual(values =colormap) +
  theme_bw() +
  theme(legend.position = "top")

# Se pierde la posibilidad de comparar ambas rectas. Se gana que las pendientes pueden sre diferentes.

## c.v ####
# Para permitir distintas pendientes pero sin perder la posibilidad de comparar deberíamos
# ajustar un modelo con interacción

model_salary_interac <- lm(data = data_salary,
                           salary ~ year * sex)
summary(model_salary_interac)

## d ####
model_salary_rank <- lm(data = data_salary,
                        salary ~ sex + year + rank)
summary(model_salary_rank)
confint(model_salary_rank)

model.matrix(model_salary_rank)

predict_tbl <- tibble(year = c(10, 10, 10, 10),
                      sex = c("Mujer", "Hombre", "Mujer", "Hombre"),
                      rank = c("Assistant Professor", "Assistant Professor",
                               "Associate Professor", "Associate Professor")) 

predicted <- predict_tbl %>%
  mutate(pred_salary = predict(model_salary_rank, newdata = predict_tbl))
predicted
