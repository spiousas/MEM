pacman::p_load(tidyverse, here, tidymodels, vip, discrim, caret)

# Leer los datos ####
DMD_normales <- read_delim(here("taller_de_datos/entrega2/data/Distrofia-Data_normales.txt"), 
                           col_names = F) %>%
  dplyr::select(c("X6", "X7", "X9", "X10", "X11", "X12", "X13")) %>%
  rename("edad" = "X6",
         "mes" = "X7",
         "anio" = "X9",
         "CK" = "X10",
         "H" = "X11",
         "PK" = "X12",
         "LD" = "X13") %>%
  mutate(anio = parse_number(anio),
         LD = parse_number(LD),
         portadora = 0)

DMD_portadoras <- read_delim(here("taller_de_datos/entrega2/data/Distrofia-Data_portadoras.txt"), 
                             col_names = F) %>%
  dplyr::select(c("X6", "X7", "X9", "X10", "X11", "X12", "X13")) %>%
  rename("edad" = "X6",
         "mes" = "X7",
         "anio" = "X9",
         "CK" = "X10",
         "H" = "X11",
         "PK" = "X12",
         "LD" = "X13") %>%
  mutate(anio = parse_number(anio),
         portadora = 1)

DMD_total <- DMD_normales %>%
  bind_rows(DMD_portadoras) %>% 
  mutate(across(everything(),  ~ case_when(.x >=0 ~ .x))) %>%
  mutate(portadora = as.factor(portadora)) %>%
  group_by(portadora) %>%
  mutate(PK = replace_na(PK, mean(PK, na.rm = T)),
         LD = replace_na(LD, mean(LD, na.rm = T)))

summary(DMD_total)

DMD_total %>% ggplot(aes(x = portadora, fill = as.factor(mes))) +
  geom_bar(position = position_dodge2())

# Me quedo sólo con las columnas uqe vamos a usar para modelar.
DMD_model <- DMD_total %>%
  dplyr::select(-c("edad", "mes", "anio"))

# Dividimos el dataset y generamos los folds
set.seed(123)
split <- initial_split(DMD_model, 
                       strata = portadora, 
                       prop = 2/3)
training <- training(split)
testing <- testing(split)

set.seed(123)
folds <- vfold_cv(training, 
                  v = 10,
                  strata = portadora)

# GLM ####
lambdas <- 10 ^ seq(-1, -3, length = 50)
ps <- seq(0.2,0.8,0.1)
pred_glm_lasso <- function(p, lambda, folds) {
  error_cv <- rep(0,10)
  f1_cv <- rep(0,10)
  for (i in 1:10) {
    fold <-folds$splits[[i]]
    data_train <- analysis(fold)
    data_val <- assessment(fold)
    weights <- if_else(data_train[,5]==1, 1-134/209, 1-75/209)
    ajuste <- glmnet(as.matrix(data_train[,1:4]), 
                     as.matrix(data_train[,5]), 
                     lambda = lambda, 
                     family = "binomial",
                     weights = weights)
    prob <- predict(ajuste, newx=as.matrix(data_val[,1:4]), s=lambda,type = 'response')
    pred <- ifelse( prob > p, 1, 0)
    conf <- confusionMatrix(as.factor(pred),as.factor(data_val$portadora))
    f1_cv[i] <-conf$table[1,1]/(conf$table[1,1]+1/2*(conf$table[2,1]+conf$table[1,2]))
  }
  return(mean(f1_cv))
}

f1_clas <- tibble(p = numeric(), lambda = numeric(), f1 = numeric())
for (j in 1:length(ps)) {
  for (l in 1:length(lambdas)) {
    metricas_p <-pred_glm_lasso(ps[j],lambdas[l], folds)
    f1_clas <- f1_clas %>% add_row(tibble(p = ps[j], lambda = lambdas[l], f1 = metricas_p))
  }
}

f1_clas %>%
  ggplot(aes(x = p,
             y = lambda,
             fill = f1)) +
  geom_tile() +
  coord_trans(y ='log')
  
    
max(f1_clas)
which(f1_clas==max(f1_clas),arr.ind = TRUE)



# KNN ####
DMD_rec <- recipe(portadora ~ ., data = training)

knn_spec <- nearest_neighbor(mode = "classification",
                             engine = "kknn",
                             neighbors = tune(),
                             weight_func = NULL,
                             dist_power = NULL)

tune_wf_knn <- workflow() %>%
  add_recipe(DMD_rec) %>%
  add_model(knn_spec) 

knn_grid <- grid_regular(
  neighbors(range = c(1,20)),
  levels = 20
)

tune_res_knn <- tune_grid(
  tune_wf_knn,
  resamples = folds,
  metrics = metric_set(accuracy, roc_auc, f_meas),
  grid = knn_grid
)

tune_res_knn %>% collect_metrics() %>%
  dplyr::filter(.metric == "f_meas") %>%
  arrange(desc(mean))

tune_res_knn %>% 
  collect_metrics() %>%
  filter(.metric == "f_meas") %>%
  dplyr::select(mean, neighbors) %>%
  ggplot(aes(x = neighbors,
             y = mean)) +
  geom_line(alpha = .5, linewidth = 1.5) +
  geom_point(show.legend = F) +
  theme_minimal()

# Random Forest ####
DMD_rec <- recipe(portadora ~ ., data = training)

rf_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune(),
  ) %>%
  set_mode("classification")

rf_grid <- grid_regular(
  mtry(range = c(1,4)),
  min_n(range = c(1,40)),
  levels = 40
)

tune_wf_rf <- workflow() %>%
  add_recipe(DMD_rec) %>%
  add_model(rf_spec) 

tune_res_rf <- tune_grid(
  tune_wf_rf,
  resamples = folds,
  metrics = metric_set(accuracy, roc_auc, f_meas),
  grid = rf_grid
)

tune_res_rf %>% collect_metrics() %>%
  dplyr::filter(.metric == "f_meas") %>%
  arrange(desc(mean))

tune_res_rf %>% select_best("f_meas")

tune_res_rf %>% 
  collect_metrics() %>%
  filter(.metric == "f_meas") %>%
  dplyr::select(mean, min_n, mtry) %>%
  ggplot(aes(x = min_n,
             y = mean,
             color = as.factor(mtry))) +
  geom_line(alpha = .5, linewidth = 1.5) +
  geom_point(show.legend = F) +
  facet_wrap(~mtry, scale = "free_x", labeller = label_both) +
  theme_minimal()

# El mejor modelo en test ####

# Cómo acmbian las métricas con 2 variables ####





