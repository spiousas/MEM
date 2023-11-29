# Regresión de Poisson ####
# -Comparar las predicciones del modelo lineal y el modelo de
# Poisson graficando los valores predichos de cada uno. ¿Que
# observa?
pacman::p_load(ISLR2, tidyverse, patchwork)

lin_mod <- lm(data = Bikeshare, formula = bikers ~ mnth + hr + workingday + temp + weathersit)
summary(lin_mod)

poisson_mod <- glm(data = Bikeshare, formula = bikers ~ mnth + hr + workingday + temp + weathersit, 
                  family=poisson )
summary(poisson_mod)

Bikeshare_mod <- Bikeshare %>%
  mutate(lin_pred = lin_mod$fitted.values,
         poisson_pred = poisson_mod$fitted.values)


Bikeshare_mod %>% 
  ggplot(aes(x = bikers,
             y = lin_pred)) +
  geom_point(alpha = .5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  theme_bw()
  
Bikeshare_mod %>% 
  ggplot(aes(x = bikers,
             y = poisson_pred)) +
  geom_point(alpha = .5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  theme_bw()

# - Predecir la cantidad total de bicicletas usadas a lo largo de un
# día laborable de Julio que está despejado.
poisson_mod2 <- glm(data = Bikeshare, formula = bikers ~ mnth + workingday + weathersit, 
                    family=poisson )

new_data <- tibble(mnth = "July", workingday = 1, weathersit = "clear")
predict(poisson_mod2, newdata = new_data, type = "response") * 24

# Clasificación ####
## GLM ####
# Responder las siguientes preguntas para los datos de default.
pacman::p_load(FNN, ISLR2, tidyverse, patchwork)

# • Realizar gráficos exploratorios para entender los datos de
# Default.

# Algunas figuritas descriptivas
ggplot(Default, aes(x = balance, y = income, color = default, pch =student)) +
  geom_point(alpha = .5) + 
  ggtitle('Default data set') +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot(Default, aes(x = default , y = balance)) +
  geom_jitter(width = .2, alpha = .1) +
  geom_boxplot(alpha = 0.1, color = "steelblue", fill = "steelblue") +
  ggtitle('Distribucion del balance por default') +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot(Default, aes(x = default , y = balance)) +
  geom_jitter(width = .2, alpha = .1) +
  geom_boxplot(alpha = 0.1, color = "steelblue", fill = "steelblue") +  
  ggtitle('Distribucion del balance por default y estudiante') + 
  facet_wrap( ~ student) +
  theme_minimal() +
  theme(legend.position = "bottom")

# • El balance ayuda a explicar el default?
ggplot(Default, aes(x = default , y = balance)) +
  geom_jitter(width = .2, alpha = .1) +
  geom_boxplot(alpha = 0.1, color = "steelblue", fill = "steelblue") +  
  ggtitle('Distribucion del balance por default y estudiante') + 
  theme_minimal() +
  theme(legend.position = "bottom")

# Pareciera que balance es un buen candidato para predecir Default

# • Ser estudiante, aumenta o disminuye la probabilidad de default?
Default %>%
  group_by(student) %>%
  summarise(m_default = mean(default == "Yes"))

# Pareciera que ser estudiante aumenta tu riesgo de default

# • Existe alguna interacción entre balance e ingreso para explicar
# el default?
ggplot(Default, aes(x = balance , y = income, color = default)) +
  geom_point(size = 1, alpha = .5) +
  ggtitle('Distribucion del balance por default y estudiante') + 
  theme_minimal() +
  theme(legend.position = "bottom")

interact_fit <- glm(default ~ balance * income, 
                     data = Default, 
                     family = binomial)
summary(interact_fit) # La interacción no da significativa

# • Con el modelo ajustado
#   default ∼ balance + student
# Grafique la curvas de probabilidad de default en función de
# balance para estudiantes y no estudiantes.
balancest_fit <- glm(default ~ balance + student, 
                     data = Default, 
                     family = binomial)

preds <- expand_grid(balance = min(Default$balance):max(Default$balance),
                     student = unique(Default$student))
preds$p_default <- predict(balancest_fit, newdata = preds, type = "response")

preds %>% ggplot(aes(x = balance,
                     y = p_default, 
                     color = student)) +
  geom_line(linewidth = 1, alpha = .5) +
  theme_bw() +
  labs(x = "Balance", y = "P(default|balance,student)", color = "Es estudiante?") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")

## GLM vs. KNN ####
# Con los datos de balance Ajustar KNN con K = 3 escalando
# apropiadamente las variables balance e ingreso. Comparar con
# los ajustes sin escalar.
Default_knn <- Default %>%
  mutate(balance_sc = scale(balance),
         income_sc  = scale(income))

Predict_x        <- crossing(balance = seq(min(Default_knn$balance), max(Default_knn$balance), length.out = 51), 
                             income = seq(min(Default_knn$income), max(Default_knn$income), length.out = 51))
Predict_x_scaled <- crossing(balance_sc = seq(min(Default_knn$balance_sc), max(Default_knn$balance_sc), length.out = 51), 
                             income_sc = seq(min(Default_knn$income_sc), max(Default_knn$income_sc), length.out = 51))

Predict_x$k3        <- knn(train = Default_knn %>% select(balance, income), 
                           test = Predict_x %>% select(balance, income),
                           cl = Default_knn %>% pull(default), k = 3 )
Predict_x_scaled$k3 <- knn(train = Default_knn %>% select(balance_sc, income_sc), 
                           test = Predict_x_scaled %>% select(balance_sc, income_sc),
                           cl = Default_knn %>% pull(default), k = 3 )

# Las regiones predichas
p1 = ggplot(Predict_x, aes( x = balance, y = income , fill = k3)) + 
  geom_tile(alpha = .7) +
  ggtitle('Regiones de clasificacion K = 3') +
  theme_bw() +
  theme(legend.position = "bottom")

p2 = ggplot(Predict_x_scaled, aes( x = balance_sc, y = income_sc , fill = k3)) + 
  geom_tile(alpha = .7) +
  ggtitle('Regiones de clasificacion K = 3 (datos escalados)') +
  theme_bw() +
  theme(legend.position = "bottom")
p1 + p2

# Al escalar logramos que las regiones sean más razonables porque nos sacamos de 
# encima las diferencias de escala entre las variables

# • Ajustar una regresión logística con
#     default ∼ balance + student + student*balance
# y otra para
#     default ∼ balance
# pero usando solo los datos de estudiantes.
#
# Con ambos modelos prediga la probabilidad de default para
# estudiantes con distintos balances. ¿
#   ¿Cambian las predicciones?
#   ¿Que está sucediendo?
balancest_fit <- glm(default ~ balance + student + student*balance, 
                     data = Default, 
                     family = binomial)

balance_fit <- glm(default ~ balance, 
                     data = Default %>% filter(student == "Yes"), 
                     family = binomial)

preds <- expand_grid(balance = min(Default$balance):max(Default$balance),
                     student = unique(Default$student))
preds$p_default <- predict(balancest_fit, newdata = preds, type = "response")

preds_st <- expand_grid(balance = min(Default$balance):max(Default$balance))
preds_st$p_default <- predict(balance_fit, newdata = preds_st, type = "response")

preds %>% 
  ggplot(aes(x = balance,
             y = p_default, 
             color = student)) +
  geom_line(linewidth = 2, alpha = .5) +
  geom_line(data = preds_st, color = "black", linewidth = 2, alpha = .5, linetype = "dashed") +
  theme_bw() +
  labs(x = "Balance", y = "P(default|balance,student)", color = "Es estudiante?") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")

# Ambas predicciones san iguales. Puede verse que los parámetros son, de hecho, iguales.
balancest_fit$coefficients[1] + balancest_fit$coefficients[3]
balance_fit$coefficients[1]

balancest_fit$coefficients[2] + balancest_fit$coefficients[4]
balance_fit$coefficients[2]