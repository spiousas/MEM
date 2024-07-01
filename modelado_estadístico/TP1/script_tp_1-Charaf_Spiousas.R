# Preliminares ####

# Leemos los paquetes
pacman::p_load(tidyverse, here, modelsummary, tidymodels, MASS, rstanarm, rstan)

# Cargamos los datos
cuestionario_tbl <- read_tsv(here("modelado_estadístico/TP1/data/data.csv"))

# Creamos una versión long del dataset
cuestionario_tbl_pivoted <- cuestionario_tbl |>
  pivot_longer(
    cols = starts_with("Q"),
    names_to = "Pregunta",
    values_to = "Respuesta",
  ) 

# Usamos la función datasummary_skim de modelsummary para ver los datos
datasummary_skim(cuestionario_tbl_pivoted, type = "numeric")

# Nos quedamos con edades menores a 90 y respuestas mayores a 0
cuestionario_tbl <- cuestionario_tbl |>
  # Filtramos todas las columnas que empiezan con Q
  filter(if_all(starts_with('Q'), function(x) x > 0)) |> 
  filter(age <= 90)

# Volvemos a generar la versiòn long de los datos
cuestionario_tbl_pivoted <- cuestionario_tbl |>
  pivot_longer(
    cols = starts_with("Q"),
    names_to = "Pregunta",
    values_to = "Respuesta",
  ) 

# Histograma de las edades
cuestionario_tbl |>
  ggplot(aes(x = age)) +
  geom_histogram(aes(y = after_stat(density)), fill = "steelblue", alpha = .4) +
  geom_density(color = "steelblue", alpha = 0, bw = 1, linewidth = 1) +
  labs(x = "Edad", y = "Densidad") +
  theme_bw()

# 1 - Dividir los datos en train y test ####
# Vamos a usar la función initial_split de tidymodels
cuestionario_splits <- initial_split(cuestionario_tbl, prop = 2/3, strata = age)
train_data <- training(cuestionario_splits)
test_data <- testing(cuestionario_splits)

# 2 - Quedarme con una pregunta interesante ####
# Quiero ver cómo se relacionan las preguntas con la edad.
cuestionario_tbl_pivoted |>
  mutate(Pregunta = as_factor(Pregunta)) |> # Para que me haga el facet_wrap
  #                                             ordenado
  ggplot(aes(x = age,
             y = Respuesta)) +
  stat_summary(color = "steelblue", alpha = .5, size = .1, fun.data = "mean_se") +
  facet_wrap(~ Pregunta, labeller = label_both) +
  labs(x = "Edad") +
  theme_bw() +
  theme(strip.background = element_rect(colour="white", fill="white"))

cuestionario_tbl |>
  dplyr::select(c("age", "Q6")) |>
  mutate(Q6 = as_factor(Q6)) |> # Para que me haga el facet_wrap ordenado
  ggplot(aes(x = Q6,
             y = age,
             group = Q6)) +
  ggdist::stat_halfeye(
    fill = "steelblue",
    alpha = .7, 
    adjust = 3,
    width = .7, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(color = "steelblue",
               fill = "steelblue",
               alpha = .4,
               width = .25,
               outlier.shape = NA) +
  labs(x = "Respuesta a Q6", y = "Edad") +
  theme_bw() 

# 5- Predecir la Q elegida (Q6 en nuestro caso) ####
train_data_fit <- train_data |>
  dplyr::select(c("age", "Q6")) |>
  mutate(Q6 = as.factor(Q6))
ajuste_ord_Q6 <- polr(Q6 ~ age, data = train_data_fit, method = "probit")

# Vemos como clasifica
pred_ord_Q6 <- tibble(age = min(cuestionario_tbl$age):max(cuestionario_tbl$age),
                      Prediccion = predict(ajuste_ord_Q6, 
                                           newdata = tibble(age = min(cuestionario_tbl$age):max(cuestionario_tbl$age))))

pred_ord_Q6 |> group_by(Prediccion) |>
  summarise(Cantidad = n()) |>
  tt()

# Predigo las Ps
pred_P_ord_Q6 <- 
  tibble(age = min(cuestionario_tbl$age):max(cuestionario_tbl$age)) |>
  bind_cols(as_tibble(predict(ajuste_ord_Q6, 
                              newdata = tibble(age = min(cuestionario_tbl$age):max(cuestionario_tbl$age)), 
                              type = "p"))) |>
  pivot_longer(cols = -age, values_to = "probabilidad", names_to = "clase")

# La figura de las Ps y las clases
pred_P_ord_Q6 |>
  ggplot(aes(x = age,
             y = probabilidad)) +
  geom_line(data = pred_ord_Q6, 
            aes(x = age, y = .6, color = Prediccion),
            linewidth = 2, show.legend = FALSE) +
  geom_text(data = tibble(x = c(26, 42, 67.5), y = .63, 
                          label = paste0("Clase = ", c(5, 4, 1))), 
            aes(x = x, y = y, label = label)) +
  geom_line(aes(color = clase)) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Edad", y = "Probabilidad", color = "Clase") +
  theme_bw() +
  theme(legend.position = "top")

# 6- Estimar que una persona de 25 años esté al menos de acuerdo con la pregunta "Me gustan las armas" (Q9) ####
train_data_fit <- train_data |>
  dplyr::select(c("age", "Q9")) |>
  mutate(Q9 = as.factor(Q9))
ajuste_ord_Q9 <- polr(Q9 ~ age, data = train_data_fit, method = "probit")

data_25 <- tibble(age=25)
pred_25 <- predict(ajuste_ord_Q9, newdata = data_25, type = "p")
proba_acuerdo <- pred_25[4]+pred_25[5]

# 7- Definir la loss function ####
loss_abs <- function(y_hat, y) {
  mean(abs(y - y_hat))
}

# 8- Ajustar un modelo lineal ####
train_data_fit <- train_data |>
  dplyr::select(c("age", "Q6")) 
ajuste_lin_Q6 <- lm(Q6 ~ age, data = train_data_fit)

# Creamos una función que hace el redondeo pero tiene min y max
round_Q <- function(x, xmin, xmax) {
  x <- round(x)
  for (i in 1:length(x)) {
    if (x[i] < xmin) {
      x[i] <- xmin
    } else if (x[i] > xmax) {
      x[i] <- xmax
    }
  }
  x
}

# Hacemos las predicciones de lm y polr
pred_lm_Q6 <- tibble(age = min(cuestionario_tbl$age):max(cuestionario_tbl$age)) |>
  # Encuentro el entero más cercano entre 1 y 5
  rowwise() |>
  mutate(lm = round_Q(predict(ajuste_lin_Q6, newdata = tibble(age = age)), 1, 5), 
         polr = as.numeric(predict(ajuste_ord_Q6, newdata = tibble(age = age))))

# Y las graficamos
pred_lm_Q6 |>
  pivot_longer(cols = -age, names_to = "Metodo", values_to = "Clase") |>
  ggplot(aes(x = age, y = Clase, color = Metodo)) +
  geom_line(linewidth = 2) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Edad", y = "Clase predicha", color = "Método") +
  theme_bw() +
  theme(legend.position = "top")

# 9- Hago las predicciones y calculo la pérdida ####
# Predecimos para ambos modelos con el conjunto de test
pred_test_data <- test_data |>
  dplyr::select(c("age", "Q6")) |>
  mutate(lm = round_Q(predict(ajuste_lin_Q6, newdata = tibble(age = age)), 1, 5), 
         polr = as.numeric(predict(ajuste_ord_Q6, newdata = tibble(age = age))))

# Calculamos la loss para cada predicción
pred_test_data |>
  pivot_longer(cols = c("lm", "polr"), names_to = "Metodo", values_to = "Clase") |>
  group_by(Metodo) |>
  summarise(loss = round(loss_abs(Clase, Q6), 3)) |>
  tt()

# 10- La versión bayesiana ####
# Filtramos los datos de training
train_data_fit <- train_data |>
  dplyr::select(c("age", "Q6")) |>
  mutate(Q6 = as.factor(Q6))

# Ajustamos un modelos bayesiano con prior R2(.00001, "mean")
ajuste_ord_bayesiano_Q6_Rchico <- stan_polr(Q6 ~ age, data = train_data_fit, method = "probit",
                                            prior = R2(.00001, "mean"), seed = 12345,
                                            algorithm = "fullrank") # for speed only
# Guardamos el modelo
saveRDS(ajuste_ord_bayesiano_Q6_Rchico, file = here("modelado_estadístico/TP1/modelos/ajuste_ord_bayesiano_Q6_Rchico.rds"))

ajuste_ord_bayesiano_Q6_Rgrande <- stan_polr(Q6 ~ age, data = train_data_fit, method = "probit",
                                            prior = R2(.999, "mean"), seed = 12345, 
                                            algorithm = "fullrank") # for speed only
# Guardamos el modelo
saveRDS(ajuste_ord_bayesiano_Q6_Rgrande, file = here("modelado_estadístico/TP1/modelos/ajuste_ord_bayesiano_Q6_Rgrande.rds"))

# Un summary de los modelos
modelsummary(list("R grande" = ajuste_ord_bayesiano_Q6_Rgrande,
                  "R chico" = ajuste_ord_bayesiano_Q6_Rchico),
             fmt = 4)

# Una figura comparando las posterioris de beta_age
as_tibble(as.matrix(ajuste_ord_bayesiano_Q6_Rchico)) |>
  mutate(R = "Chico") |>
  bind_rows(as_tibble(as.matrix(ajuste_ord_bayesiano_Q6_Rgrande)) |>
              mutate(R = "Grande")) |>
  ggplot(aes(x = age, fill = R)) +
  geom_density(color = NA, alpha = .6) +
  scale_fill_brewer(palette = "Accent") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Beta", y = NULL) +
  theme_bw()
    
# Ajustamos los mismos modelos pero con una muestra aleatoria de 5000 filas de los datos de entrenamiento
set.seed(12345)
train_data_fit_short <- sample_n(train_data_fit, size = 5000)

# Volvemos a ajustar los modelos
ajuste_ord_bayesiano_Q6_Rchico_menosdatos <- stan_polr(Q6 ~ age, data = train_data_fit_short, 
                                                        method = "probit",
                                                        prior = R2(.00001, "mean"), seed = 12345,
                                                        algorithm = "fullrank")
# Guardamos el modelo
saveRDS(ajuste_ord_bayesiano_Q6_Rchico_menosdatos, file = here("modelado_estadístico/TP1/modelos/ajuste_ord_bayesiano_Q6_Rchico_menosdatos.rds"))

ajuste_ord_bayesiano_Q6_Rgrande_menosdatos <- stan_polr(Q6 ~ age, data = train_data_fit_short, 
                                             method = "probit",
                                             prior = R2(.99, "mean"), seed = 12345, 
                                             algorithm = "fullrank")
# Guardamos el modelo
saveRDS(ajuste_ord_bayesiano_Q6_Rgrande_menosdatos, file = here("modelado_estadístico/TP1/modelos/ajuste_ord_bayesiano_Q6_Rgrande_menosdatos.rds"))

# Un summary de los modelos
modelsummary(list("R grande" = ajuste_ord_bayesiano_Q6_Rchico_menosdatos,
                  "R chico" = ajuste_ord_bayesiano_Q6_Rgrande_menosdatos),
             fmt = 4)

as_tibble(as.matrix(ajuste_ord_bayesiano_Q6_Rchico_menosdatos)) |>
  mutate(R = "Chico") |>
  bind_rows(as_tibble(as.matrix(ajuste_ord_bayesiano_Q6_Rgrande_menosdatos)) |>
              mutate(R = "Grande")) |>
  ggplot(aes(x = age, fill = R)) +
  geom_density(color = NA, alpha = .6) +
  scale_fill_brewer(palette = "Accent") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Beta", y = NULL) +
  theme_bw()

train_data_fit <- train_data |>
  dplyr::select(c("age", "Q6"))

# 11 - Modelo de reg ordinal Bayesiano con rstan
# En el archivo punto11.stan se encuentran todas las especificaciones del modelo. En este caso consideramos 
#como funcion F la funcion logistica y pusimos una distribucion a priori para el parametro beta N(0,10).
N<- 5000
K<- 5

train_data_fit <- train_data |>
  dplyr::select(c("age", "Q6"))
set.seed(12345)
train_data_fit_short <- sample_n(train_data_fit, size = N)
y <- train_data_fit_short$Q6
x <- train_data_fit_short$age

# A continuacion ajustamos el modelo y realizamos los histogramas para ver la distribucion
# a posteriori de los parametros (beta y los puntos de corte llamados c_i).
ajuste_ord_stan <- stan(file = "modelado_estadístico/TP1/modelos/punto11.stan",
                        data = c('N','K','y','x'), iter = 1000)
# Guardamos el modelo
saveRDS(ajuste_ord_stan, file = here("modelado_estadístico/TP1/modelos/ajuste_ord_stan.rds"))
# Cargamos el modelo
ajuste_ord_stan <- readRDS(here("modelado_estadístico/TP1/modelos/ajuste_ord_stan.rds"))

# Tabla de las estimaciones puntuales
modelsummary(list("Stan" = ajuste_ord_stan),
             fmt = 4,
             title = "Estimaciones puntuales del modelo ajustado utilizando Stan.")

# Figura de las distribuciones a posteriori
as_tibble(as.matrix(ajuste_ord_stan)) |>
  select(-lp__) |>
  pivot_longer(cols = everything(),
               names_to = "param",
               values_to = "value") |>
  ggplot(aes(x = value)) +
  geom_density(fill = "steelblue", alpha = .6) +
  facet_wrap(~param, scales = "free") +
  labs(x = "Estimaciones", y = NULL) +
  theme_bw() +
  theme(legend.position = "top") +
  theme(strip.background = element_rect(colour="white", fill="white"))

# Ajuste del modelo usando polr para comparar
train_data_fit <- train_data |>
  dplyr::select(c("age", "Q6")) |>
  mutate(Q6 = as.factor(Q6))
ajuste_ord_Q6_l <- polr(Q6 ~ age, data = train_data_fit)
