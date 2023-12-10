pacman::p_load(ISLR2, tidyverse, gam, patchwork, rsample, splines, gam, GGally)

# Smoothing splines ####
Wage %>% ggplot(aes(x = age,
           y = wage)) +
  geom_point(alpha = .5) +
  theme_bw()
  
# con grados de libertad fijados, que es lo ismo que asociar un lambda
# Los grados de libertad pueden ir de 2 a n
fit_smoothsplines <- smooth.spline(Wage$age, Wage$wage, df = 7)
fit_smoothsplines

# con cross validation para elegir el mejor lambda
fit_smoothsplines_cv <- smooth.spline(Wage$age, Wage$wage, cv = T)
fit_smoothsplines_cv$df

preds <- as_tibble(predict(fit_smoothsplines, 18:80))

Wage %>% ggplot(aes(x = age,
                    y = wage)) +
  geom_point(alpha = .5) +
  geom_line(data = preds, aes(x=x, y=y), color = "brown", linewidth = 2) +
  theme_bw()

# GAMs Ejercicio 6 ####
## Con polinomio ####
k <- 10
# Inicializo matriz de métricas
metricas <- tibble(grado = numeric(),
                   MSE   = numeric())

set.seed(1234)
folds <- vfold_cv(Wage, v = k, strata = wage)
folds

# Grados del polinomio
m <- 1:20

for (i in m) { # Loop en m
  err <- c()
  for (j in 1:k) { # Cross-validation
    
    # Los folds
    fold<- folds$splits[[j]]
    
    train       <- analysis(fold)
    validation  <- assessment(fold)
    
    model <- lm(wage ~ poly(age, i), data = train)
    pred  <- predict(model, newdata = validation %>% select(-wage), type = "response")
    err   <- append(err, mean((validation %>% pull(wage) - pred)^2))
  }
  
  metricas <- metricas %>% bind_rows(tibble(grado = i, MSE = mean(err)))
}

metricas %>%
  ggplot(aes(x = grado,
             y = MSE)) +
  geom_point(size = 2, color = "brown") +
  labs(title = "MSE en función del grado del polinomio",
       x = "Grado del polinomio",
       y = "MSE de validación cruzada") +
  geom_line(linewidth = 2, alpha = 0.5, color = "brown") +
  theme_bw()

# Esto de anova lo vi en los ejercicios resueltos:
models <- list(lm(wage ~ age, data = Wage))

for (i in m) {
  models[[i+1]] = lm(wage ~ poly(age, i), data = Wage)
}

anova(models[[1]], models[[2]], models[[3]], models[[4]], models[[5]], models[[6]],
      models[[7]], models[[8]], models[[9]], models[[10]], models[[11]], models[[12]],
      models[[13]], models[[14]], models[[15]], models[[16]], models[[17]], models[[18]])
# Se ve que la diff significativa es para 9 también.

metricas %>% filter(MSE == min(MSE))
# Elijo polinomio de grado 9

best_grado <- metricas %>% filter(MSE == min(MSE)) %>% pull(grado)

model_final <- lm(wage ~ poly(age, best_grado), data = Wage)
pred_poly <- tibble(age = 18:80) %>%
  mutate(pred_wage = predict(model_final, newdata = tibble(age), type = "response"))

Wage %>% 
  ggplot(aes(x = age,
             y = wage)) +
  geom_point(alpha = .5) +
  geom_line(data = pred_poly, aes(y = pred_wage),
            linewidth = 2, color = "dodgerblue4") +
  labs(title = "Predicción con polinomio de grado 9",
       x = "Edad",
       y = "Salario") +
  theme_bw()

## Con escalones ####
k <- 10
# Inicializo matriz de métricas
metricas <- tibble(cuts = numeric(),
                   MSE  = numeric())

# Grados del polinomio
cuts <- 2:20

for (i in cuts) { # Loop en m
  err <- c()
  for (j in 1:k) { # Cross-validation
    set.seed(1234)
    folds <- vfold_cv(Wage %>% mutate(age_cut = cut(age, i)), v = k, strata = wage)
    
    # Los folds
    fold <- folds$splits[[j]]
    
    train       <- analysis(fold) 
    validation  <- assessment(fold)
    
    model <- lm(wage ~ age_cut, data = train)
    pred  <- predict(model, newdata = validation %>% select(-wage), type = "response")
    err   <- append(err, mean((validation %>% pull(wage) - pred)^2))
  }
  
  metricas <- metricas %>% bind_rows(tibble(cuts = i, MSE = mean(err)))
}

metricas %>%
  ggplot(aes(x = cuts,
             y = MSE)) +
  geom_point(size = 2, color = "brown") +
  labs(title = "MSE en función de la cantidad de cortes",
       x = "Cantidad de cortes",
       y = "MSE de validación cruzada") +
  geom_line(linewidth = 2, alpha = 0.5, color = "brown") +
  theme_bw()

metricas %>% filter(MSE == min(MSE))
# Con 11 cortes es lo mejor

best_cut <- metricas %>% filter(MSE == min(MSE)) %>% pull(cuts)
best_cut

model_final <- lm(wage ~ age_cut, data = Wage %>% mutate(age_cut = cut(age, best_cut)))

pred_escalon <- tibble(age = 18:80) %>%
  mutate(age_cut = cut(age, best_cut)) %>%
  mutate(pred_wage = predict(model_final, newdata = tibble(age_cut), type = "response"))

Wage %>% 
  ggplot(aes(x = age,
             y = wage)) +
  geom_point(alpha = .5) +
  geom_line(data = pred_escalon, aes(y = pred_wage),
            linewidth = 2, color = "dodgerblue4") +
  labs(title = "Predicción con escalones (11 cortes)",
       x = "Edad",
       y = "Salario") +
  theme_bw()

## Ploteo ambas predicciones
Wage %>% 
  ggplot(aes(x = age,
             y = wage)) +
  geom_point(alpha = .5) +
  geom_line(data = pred_escalon, aes(y = pred_wage),
            linewidth = 2, color = "dodgerblue4") +
  geom_line(data = pred_poly, aes(y = pred_wage),
            linewidth = 2, color = "brown") +
  labs(title = "Predicción con ambos métodos",
       x = "Edad",
       y = "Salario") +
  theme_bw()

# GAMs Ejercicio 8 ####
ggpairs(Auto %>% select(where(is.numeric)))

# hago un smoothing splines para cada variable
gam.todas <- gam(mpg ~ s(displacement,df = 4 ) + s(horsepower, df=4) + s(weight, df=4) + s(acceleration, df=4) + s(year, df=4) ,
                 data = Auto)

summary(gam.todas)

pred_Auto <- Auto %>%
  mutate(pred_mpg = predict(gam.todas, newdata = tibble(.), type = "response"))

par(mfrow=c(2,3))
plot.Gam(gam.todas, se = TRUE, col = "red")
