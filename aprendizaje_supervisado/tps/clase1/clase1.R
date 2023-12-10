# Recrear los gráficos anteriores utilizando vecinos mas cercanos con
# distintos valores de K para f(x) = 5x(x − 1)(x + 1) y una muestra
# uniforme de n = 50 en el intervalo (−1.5, 1.5).
pacman::p_load(tidyverse, FNN)

# Uso los mismos valores que Gonza para comparar
set.seed(1)
n_train <- 50 
n_test <- 500
x_min <- -1
x_max <- 1
sigma <- 1

# Funcion generadora de los datos
f_gen <- function(x){
  10*x*(x-1)*(x+1)
}

# Creo los conjuntos de entrenamiento y testeo
x_train <- runif(n_train, min = x_min, max = x_max)
y_train <- f_gen(x_train) + rnorm(n_train, mean = 0, sd = sigma)
y_real <- f_gen(x_train)
train <- tibble(x_train, y_train, y_real)

x_test <- runif(n_test, min = x_min, max = x_max)
y_test <- f_gen(x_test) + rnorm(n_test, mean = 0, sd = sigma)

# Ploteo los datos de norm versus la función generadora 
train %>%
  ggplot(aes(x = x_train,
             y = y_train)) +
  geom_point() +
  geom_line(aes(y = y_real), color = "red") +
  theme_minimal()

# Uso los mismos valores que Gonza para comparar
ks <- c(2, 5, 10)
preds <- tibble(x = seq(x_min, x_max, 0.05)) %>%
  mutate(y =  f_gen(x),
         k2  = knn.reg(train = x_train, test = tibble(x), y = y_train, k = ks[1] )$pred,
         k5  = knn.reg(train = x_train, test = tibble(x), y = y_train, k = ks[2] )$pred,
         k10 = knn.reg(train = x_train, test = tibble(x), y = y_train, k = ks[3] )$pred)

preds %>%
  pivot_longer(cols = y:k10, names_to = "vars", values_to = "val") %>%
  ggplot(aes(x = x,
             y = val,
             color = vars)) +
  geom_line() +
  geom_point(data = train,
             aes(x = x_train,
                 y = y_train),
             color = "black") +
  theme_minimal()

# Calculo el MSE
kvecs <- 2:20

mse_train_vec <- c()
mse_test_vec <- c()

for (i in 1:length(kvecs)) {
  mse_train <- mean( (y_train - knn.reg(train =x_train,test=tibble(x = x_train),y=y_train,k=kvecs[i] )$pred)^2)
  mse_test <-mean( (y_test - knn.reg(train =x_train,test=tibble(x = x_test),y=y_train,k=kvecs[i] )$pred)^2)
  
  mse_train_vec <- c(mse_train_vec,mse_train)
  mse_test_vec <- c(mse_test_vec,mse_test)
}

tibble(kvecs, mse_train_vec, mse_test_vec) %>%
  pivot_longer(cols = mse_train_vec:mse_test_vec, names_to = "cat", values_to = "val") %>%
  ggplot(aes(x = kvecs,
             y = val,
             color = cat)) +
  geom_point() +
  geom_line(linewidth = 1.5, alpha = .4) +
  labs(x = "k", y = NULL, color = NULL) +
  theme_bw() +
  theme(legend.position = "top")

# Sesgo y varianza ####
## Con el método de Manu ####
sq_bias = c()
var = c()
mse = c()

for(i in 1:length(kvecs)){
  yhat <- knn.reg(train = x_train, test = tibble(x = x_test), y = y_train, k = kvecs[i])$pred
  sq_bias[i] <- (mean (y_test -yhat) )^2
  var[i] = var(yhat-y_test)
  mse[i]=mean( (y_test - yhat)^2)
  
}

tibble(kvecs, sq_bias, var, mse) %>%
  pivot_longer(cols = sq_bias:mse, names_to = "cat", values_to = "val") %>%
  ggplot(aes(x = kvecs,
             y = val,
             color = cat)) +
  geom_point() +
  labs(x = "k", y = NULL, color = NULL) +
  geom_line(linewidth = 1.5, alpha = .4) +
  theme_bw() +
  theme(legend.position = "top")

## Con el método de ISLR ####
simulacion_k <- function(K, Nrep){
  pred_rep <- matrix(NA, Nrep, n_test)
  
  set.seed(123)
  for (i in 1:Nrep) {
    x_train <- tibble( x = runif(n_train, x_min, x_max) )
    y_train <- f_gen(x_train$x) + rnorm(n_train, sd = sigma)
    
    pred_rep[i,] <- knn.reg(x_train, test = tibble(x = x_test), y = y_train, k = K)$pred
  }
  
  k_test_mse   <- rep(NA, n_test)
  k_test_var   <- rep(NA, n_test)
  k_test_sesgo <- rep(NA, n_test)

  for (j in 1:n_test) {
    k_test_mse[j]   <- mean((y_test[j] - pred_rep[,j])^2)
    k_test_sesgo[j] <- (mean(pred_rep[,j]) - f_gen(x_test)[j])^2
    k_test_var[j]   <- var(pred_rep[,j])
  }
  
  return(c(mean(k_test_mse), mean(k_test_sesgo), mean(k_test_var)))
  
}

kvecs <- 1:20

sq_bias <- rep(0, length(kvecs))
var     <- rep(0, length(kvecs))
mse     <- rep(0, length(kvecs))

for (k in kvecs) {
  resultados <- simulacion_k(k, 1000)
  mse[k]     <- resultados[1]
  sq_bias[k] <- resultados[2]
  var[k]     <- resultados[3]
}

tibble(kvecs, sq_bias, var, mse) %>%
  pivot_longer(cols = sq_bias:mse, names_to = "cat", values_to = "val") %>%
  ggplot(aes(x = kvecs,
             y = val,
             color = cat)) +
  geom_point() +
  labs(x = "k", y = NULL, color = NULL) +
  geom_line(linewidth = 1.5, alpha = .4) +
  theme_bw() +
  theme(legend.position = "top")

# Último problema ####
f_poly <- function(x){
  x+0.1*x^(10)
  }

set.seed(1)
n_train <- 20 
n_test  <- 200
sigma   <- 1

# creo conjuntos de train y test
train <- tibble( x = runif(n_train, -1, 1) ) %>% 
  mutate(y = f_poly(x) + rnorm(n_train, sd = sigma))

test <- tibble( x = runif(n_test, -1, 1) ) %>% 
  mutate(y = f_poly(x) + rnorm(n_test, sd = sigma))

modelo_lineal <- train %>% lm(data =., y ~ x)
summary(modelo_lineal)
modelo_poly   <- train %>% lm(data =., y ~ poly(x,10))
summary(modelo_poly)

predictions <- tibble(x = seq(-1, 1, 0.05)) %>%
  mutate(y = f_poly(x),
         lin = predict(modelo_lineal, newdata = data.frame(x)),
         poly = predict(modelo_poly, newdata = data.frame(x)))

predictions %>% ggplot(aes(x = x,
                           y = lin)) +
  geom_point(data = train, aes(y = y)) +
  geom_line(linewidth = 1.5, alpha = .4) +
  geom_line(aes(y = y), linewidth = 1.5, alpha = .4, color = "blue") +
  geom_line(aes(y = poly), linewidth = 1.5, alpha = .4, color = "red") +
  theme_bw() +
  ylim(c(-2, 2)) +
  labs(x = "k", y = "y") +
  theme(legend.position = "top")

test <- test %>%
  mutate(lin = predict(modelo_lineal, newdata = data.frame(x)),
         poly = predict(modelo_poly, newdata = data.frame(x)))

MSE <- test %>%
  summarise(MSE_lin  = mean((y - lin)^2),
            MSE_poly = mean((y - poly)^2))
MSE

# Para este tamaño de conjunto de entrenamiento vemos que el MSE es más grande 
# porque, si bien tiene menos sesgo, tiene más varianza.

## Probemos con un set de entrenamiento más grande
set.seed(1)
n_train <- 10000
n_test  <- 200
sigma   <- 1

train <- tibble( x = runif(n_train, -1, 1) ) %>% 
  mutate(y = f_poly(x) + rnorm(n_train, sd = sigma))

test <- tibble( x = runif(n_test, -1, 1) ) %>% 
  mutate(y = f_poly(x) + rnorm(n_test, sd = sigma))

modelo_lineal <- train %>% lm(data =., y ~ x)
summary(modelo_lineal)
modelo_poly   <- train %>% lm(data =., y ~ poly(x,10))
summary(modelo_poly)

predictions <- tibble(x = seq(-1, 1, 0.05)) %>%
  mutate(y = f_poly(x),
         lin = predict(modelo_lineal, newdata = data.frame(x)),
         poly = predict(modelo_poly, newdata = data.frame(x)))

predictions %>% ggplot(aes(x = x,
                           y = lin)) +
  geom_point(data = train, aes(y = y), alpha = .2, size = .2) +
  geom_line(linewidth = 1.5, alpha = .4) +
  geom_line(aes(y = y), linewidth = 1.5, alpha = .4, color = "blue") +
  geom_line(aes(y = poly), linewidth = 1.5, alpha = .4, color = "red") +
  theme_bw() +
  ylim(c(-2, 2)) +
  labs(x = "k", y = "y") +
  theme(legend.position = "top")

test <- test %>%
  mutate(lin = predict(modelo_lineal, newdata = data.frame(x)),
         poly = predict(modelo_poly, newdata = data.frame(x)))

MSE <- test %>%
  summarise(MSE_lin  = mean((y - lin)^2),
            MSE_poly = mean((y - poly)^2))
MSE

# Ahora ambos tienen el mismo MSE porque, al parecerce tanto f a f_hat (para este dominio),
# el MSE es simplemente Var(epsilon), de hecho MSE aprox 1 = sigma^2