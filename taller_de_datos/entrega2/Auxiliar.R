pacman::p_load(tidyverse, here, tidymodels, vip, discrim)

# Leer los datos ####
DMD_normales <- read_delim(here("taller_de_datos/entrega2/data/Distrofia-Data_normales.txt"), 
                            col_names = F) %>%
  select(c("X6", "X7", "X9", "X10", "X11", "X12", "X13")) %>%
  rename("edad" = "X6",
         "mes" = "X7",
         "anio" = "X9",
         "CK" = "X10",
         "H" = "X11",
         "PK" = "X12",
         "LD" = "X13") %>%
  mutate(anio = parse_number(anio),
         LD = parse_number(LD),
         portadora = "No")

head(DMD_normales)
summary(DMD_normales)

DMD_portadoras <- read_delim(here("taller_de_datos/entrega2/data/Distrofia-Data_portadoras.txt"), 
                              col_names = F) %>%
  select(c("X6", "X7", "X9", "X10", "X11", "X12", "X13")) %>%
  rename("edad" = "X6",
         "mes" = "X7",
         "anio" = "X9",
         "CK" = "X10",
         "H" = "X11",
         "PK" = "X12",
         "LD" = "X13") %>%
  mutate(anio = parse_number(anio),
         portadora = "Si")


head(DMD_portadoras)
summary(DMD_portadoras)

DMD_total <- DMD_normales %>%
  bind_rows(DMD_portadoras) %>% 
  mutate(across(everything(),  ~ case_when(.x >=0 ~ .x))) %>%
  mutate(portadora = as.factor(portadora)) %>%
  group_by(portadora) %>%
  mutate(PK = replace_na(PK, mean(PK, na.rm = T)),
         LD = replace_na(LD, mean(LD, na.rm = T)))

summary(DMD_total)

# Crear Random Forest ####
DMD_model <- DMD_total %>%
  dplyr::select(-c("edad", "mes", "anio"))

## Split en train y test ####
set.seed(123)
DMD_split <- initial_split(DMD_model, strata = portadora)
DMD_train <- training(DMD_split)
DMD_test <- testing(DMD_split)

## Preprocessing ####
DMD_rec <- recipe(portadora ~ ., data = DMD_train)
DMD_prep <- prep(DMD_rec)
juiced <- juice(DMD_prep)

## Definición ####  
rf_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
  ) %>%
  set_mode("classification")

glm_spec <- logistic_reg(engine = "glmnet", 
                         penalty = tune(), 
                         mixture = tune())

knn_spec <- nearest_neighbor(mode = "classification",
                             engine = "kknn",
                             neighbors = tune(),
                             weight_func = NULL,
                             dist_power = NULL
)

dmd_models <-
  workflow_set(
    preproc = list(formula = portadora ~ .),
    models = list(
      rf = rf_spec, 
      glm = glm_spec,
      knn = knn_spec
    )
  )

tune_wf <- workflow() %>%
  add_recipe(DMD_rec) %>%
  add_model(rf_spec) 

## Train ####  
set.seed(222)
DMD_fold <- vfold_cv(DMD_train, strata = portadora)
DMD_fold

doParallel::registerDoParallel()

set.seed(345)
tune_res <- tune_grid(
  tune_wf,
  resamples = DMD_fold,
  grid = 20
)

tune_res %>% collect_metrics() 

tune_res %>% select_best("accuracy")

set.seed(428)
dmd_models_res <-
  dmd_models %>% 
  workflow_map(
    "tune_grid",
    resamples = DMD_fold,
    metrics = metric_set(accuracy, roc_auc, f_meas),
    grid = 100
  )

autoplot(dmd_models_res)

dmd_models_res %>% collect_metrics() 

rank_results(dmd_models_res, rank_metric = "f_meas") %>%
  filter(.metric == "f_meas")

dmd_models_res %>% select_best("accuracy")

## Visualizaciones ####  
tune_res %>% 
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  dplyr::select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter") %>%
  ggplot(aes(x = value,
             y = mean,
             color = parameter)) +
  geom_point(show.legend = F) +
  facet_wrap(~parameter, scale = "free_x") +
  theme_minimal()
    
rf_grid <- grid_regular(
  mtry(range = c(1,1)),
  min_n(range = c(1,40)),
  levels = 40
)

## Entrenar con una grilla regular ####
set.seed(456)
regular_res <- tune_grid(
  tune_wf,
  resamples = DMD_fold,
  grid = rf_grid,
  metrics = metric_set(roc_auc, pr_auc,
                       accuracy, f_meas)
)

regular_res %>% collect_metrics() 

regular_res %>% select_best("f_meas")


regular_res %>% 
  collect_metrics() %>%
  filter(.metric == "f_meas") %>%
  mutate(mtry = factor(mtry)) %>%
  ggplot(aes(x = min_n,
             y = mean,
             color = mtry)) +
  geom_line(alpha = .5, linewidth = 1.5) +
  geom_point() +
  theme_minimal()

## Finalizar ####
best_f1 <- select_best(regular_res, "f_meas")

final_rf <- finalize_model(
  tune_spec,
  best_f1
)
final_rf

## Entrenamiento final ####
final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(portadora ~ .,
      data = juice(DMD_prep)) %>%
  vip(geom = "col")

final_wf <- workflow() %>%
  add_recipe(DMD_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(DMD_split,
           metrics = metric_set(roc_auc, pr_auc,
                                accuracy, f_meas))

final_res %>% collect_metrics()

final_res %>% collect_predictions() %>%
  bind_cols(DMD_test)

# Balanceo de la muestra (Smote, pesos, etc.)
# 