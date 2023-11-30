# Considere un modelo de regresión logística con p = 1000 variables explicativas.
# Se sabe que el modelo es esparso, es decir solo algunos de los coeficientes son distintos de cero. Además solo se disponen n = 50
# observaciones.
# Se consideran entonces la siguiente familia de metodologías de ajuste: Para cada m entre 1 y 20, se elijen las m variables con mayor
# correlación con Y y se ajusta la regresión logística con las m variables seleccionadas.
# 1. Para cada 1 ≤ m ≤ 20 calcule el error de validación cruzada con K = 5 cruces.
# 2. Grafique.
# 3. ¿Qué valor de m minimiza el CV?
#   4. Repita varias veces lo anterior con nuevos cruces. Se minimiza en el mismo m?
#   5. Haga los mismo con LOOCV.
# Para la simulación genere una muestra de n = 50 con un modelo donde P(x) depende

# Con 50 datos y CV ####
# Creo los datos
set.seed(1)
nvar <- 1000
nobs <- 50

m <- c(1:20)

x <- matrix(rnorm(nobs * nvar), nrow = nobs)

p <- 1 / (1 + exp(-rowSums(x[, 1:7])))
y <- rbinom(nobs, 1, p)
df <- as_tibble(x) %>% bind_cols(y = y)

seeds <- 1:4
k <- 5
# Inicializo matriz de métricas
metricas <- tibble(m = numeric(),
                   seed = numeric(),
                   MSE = numeric())

for (seed in seeds) {
  
  set.seed(seed)
  folds <- vfold_cv(df, 
                    v = k)
  
  for (i in m) { # Loop en m
    err <- c()
    for (j in 1:k) { # Cross-validation
      
      # Los folds
      fold  <- folds$splits[[j]]
      train <- analysis(fold)
      test  <- assessment(fold)
      
      cors  <- abs(cor(train %>% select(-y), train %>% select(y)))
      cols  <- cors >= sort(cors, decreasing = TRUE)[i]
      
      model <- glm(y ~ ., data = train[,c(which(cols), nvar+1)], family = binomial)
      pred  <- predict(model, newdata = test %>% select(-y), type = "response")
      err   <- append(err, 1 - mean(test %>% pull(y) == (pred > .5)))
    }
    
    metricas <- metricas %>% bind_rows(tibble(m = i, seed = seed, MSE = mean(err)))
  }
}

metricas %>%
  ggplot(aes(x = m,
             y = MSE,
             color = as.factor(seed))) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Dark2", name = "Semilla") + 
  labs(title = "Error de clasificación con 50 datos y 5 folds (varias semillas)") +
  geom_line(linewidth = 2, alpha = 0.5) +
  theme_bw()

# Qué pasa si en lugar de 50 tengo 5000 datas ####
set.seed(1)
nvar <- 1000
nobs <- 5000

m <- c(1:20)

x <- matrix(rnorm(nobs * nvar), nrow = nobs)

p <- 1 / (1 + exp(-rowSums(x[, 1:7])))
y <- rbinom(nobs, 1, p)
df <- as_tibble(x) %>% bind_cols(y = y)

seeds <- 1:4
k <- 5
# Inicializo matriz de métricas
metricas <- tibble(m = numeric(),
                   seed = numeric(),
                   MSE = numeric())

for (seed in seeds) {
  
  set.seed(seed)
  folds <- vfold_cv(df, v = k)
  
  
  for (i in m) { # Loop en m
    err <- c()
    for (j in 1:k) { # Cross-validation
      
      # Los folds
      fold  <- folds$splits[[j]]
      train <- analysis(fold)
      test  <- assessment(fold)
      
      cors  <- abs(cor(train %>% select(-y), train %>% select(y)))
      cols  <- cors >= sort(cors, decreasing = TRUE)[i]
      
      model <- glm(y ~ ., data = train[,c(which(cols), nvar+1)], family = binomial)
      pred  <- predict(model, newdata = test %>% select(-y), type = "response")
      err   <- append(err, 1 - mean(test %>% pull(y) == (pred > .5)))
    }
    
    metricas <- metricas %>% bind_rows(tibble(m = i, seed = seed, MSE = mean(err)))
  }
}

metricas %>%
  ggplot(aes(x = m,
             y = MSE,
             color = as.factor(seed))) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Dark2", name = "Semilla") +
  labs(title = "Error de clasificación con 5000 datos y 5 folds (varias semillas)") +
  geom_line(linewidth = 2, alpha = 0.5) +
  theme_bw()

# Y si hago LOOCV ####
set.seed(1)
nvar <- 1000
nobs <- 50

m <- c(1:20)

x <- matrix(rnorm(nobs * nvar), nrow = nobs)

p <- 1 / (1 + exp(-rowSums(x[, 1:7])))
y <- rbinom(nobs, 1, p)
df <- as_tibble(x) %>% bind_cols(y = y)

seeds <- 1:4
k <- 50
# Inicializo matriz de métricas
metricas <- tibble(m = numeric(),
                   seed = numeric(),
                   MSE = numeric())

for (seed in seeds) {
  
  set.seed(seed)
  folds <- vfold_cv(df, v = k)
  
  for (i in m) { # Loop en m
    err <- c()
    for (j in 1:k) { # Cross-validation
      
      # Los folds
      fold  <- folds$splits[[j]]
      train <- analysis(fold)
      test  <- assessment(fold)
      
      cors  <- abs(cor(train %>% select(-y), train %>% select(y)))
      cols  <- cors >= sort(cors, decreasing = TRUE)[i]
      
      model <- glm(y ~ ., data = train[,c(which(cols), nvar+1)], family = binomial)
      pred  <- predict(model, newdata = test %>% select(-y), type = "response")
      err   <- append(err, 1 - mean(test %>% pull(y) == (pred > .5)))
    }
    
    metricas <- metricas %>% bind_rows(tibble(m = i, seed = seed, MSE = mean(err)))
  }
}

metricas %>%
  ggplot(aes(x = m,
             y = MSE,
             color = as.factor(seed))) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Dark2", name = "Semilla") + 
  labs(title = "Error de clasificación con 50 datos y 50 folds (varias semillas)") +
  geom_line(linewidth = 2, alpha = 0.5) +
  theme_bw()
