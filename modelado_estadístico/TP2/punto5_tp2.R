rm(list=ls())

pacman::p_load(tidyverse, ggdag, dagitty, here, modelsummary,tidymodels, patchwork, fastDummies, ggdist, lme4, mgcv)

#leemos todos los archivos
credits_train <- read_csv(here("modelado_estadistico/TP2/data/credits_train.csv"), 
                          show_col_types = FALSE, col_select = -1)

titles_train <- read_csv("titles_train.csv", 
                         show_col_types = FALSE, col_select = -1) |>
  mutate(genres = str_replace_all(genres, "\\[|\\]", ""),
         genres = str_replace_all(genres, "'", ""),
         production_countries = str_replace_all(production_countries, "\\[|\\]", ""),
         production_countries = str_replace_all(production_countries, "'", ""))

credits_test <- read_csv("credits_test.csv", 
                          show_col_types = FALSE, col_select = -1)

titles_test <- read_csv("titles_test.csv", 
                         show_col_types = FALSE, col_select = -1) |>
  mutate(genres = str_replace_all(genres, "\\[|\\]", ""),
         genres = str_replace_all(genres, "'", ""),
         production_countries = str_replace_all(production_countries, "\\[|\\]", ""),
         production_countries = str_replace_all(production_countries, "'", ""))


# Separamos en train y test
set.seed(12)
titles_splits <- initial_split(titles_train, prop = 2/3)
train_data <- training(titles_splits)
test_data <- testing(titles_splits)

#funcion de perdida
loss <- function(y_hat, y) {
  mean((y - y_hat)^2)
}

## MODELO 1 - spline con release_year, runtime y production_countries (random efects) ####

# Data con paises duplicados
train_data_by_country <- train_data |>
  separate_rows(production_countries, sep = ", ") |>
  filter(production_countries != "")  |>
  drop_na(imdb_score)

#ajuste
train <- train_data_by_country %>%
  dplyr::select(c("imdb_score", "release_year", "runtime", "production_countries"))

train$production_countries<- as.factor(train$production_countries)

fit <- gam(imdb_score ~ s(release_year, bs = 'cr') + s(runtime, bs = 'cr') +
             s(production_countries, bs='re'), data = train)

#data para testear
test_data <- test_data

test_data_by_country <- test_data |>
  separate_rows(production_countries, sep = ", ") 
  
test <- test_data_by_country %>%
  dplyr::select(c("imdb_score", "release_year", "runtime", "production_countries"))

test$production_countries<- as.factor(test$production_countries)

#predecimos
pred_country <- tibble(id=test_data_by_country$id)

pred <- predict(fit, newdata = test)

pred_country <-pred_country %>%
  mutate(pred=pred)

predicciones <- pred_country %>%
  group_by(id) %>%
  summarise(pred_prom = mean(pred, na.rm = TRUE)) %>%
  mutate(original_order=match(id,test_data$id)) %>%
  arrange(original_order)


error1 <- loss(predicciones$pred_prom,test_data$imdb_score)

## MODELO 2 - spline con release_year, runtime y genero (random efects)

#data con generos duplicados
train_data_by_genre <- train_data |>
  separate_rows(genres, sep = ", ") |>
  filter(genres != "") |>
  drop_na(imdb_score)

#ajuste
train <- train_data_by_genre %>%
  dplyr::select(c("imdb_score", "release_year", "runtime", "genres"))

train$genres<- as.factor(train$genres)

fit <- gam(imdb_score ~ s(release_year, bs = 'cr') + s(runtime, bs = 'cr') +
             s(genres, bs='re'), data = train)

#data para testear
test_data<- test_data |>
  drop_na(imdb_score)

test_data_by_genre<- test_data |>
  separate_rows(genres, sep = ", ") 


test<- test_data_by_genre %>%
  dplyr::select(c("imdb_score", "release_year", "runtime", "genres"))

test$genres<- as.factor(test$genres)

#predecimos
pred_country<-tibble(id=test_data_by_genre$id)

pred<- predict(fit, newdata = test)

pred_country <-pred_country %>%
  mutate(pred=pred)

predicciones <- pred_country %>%
  group_by(id) %>%
  summarise(pred_prom = mean(pred, na.rm = TRUE)) %>%
  mutate(original_order=match(id,test_data$id)) %>%
  arrange(original_order)


error2 <- loss(predicciones$pred_prom,test_data$imdb_score)

## MODELO 3 que se podria probar - spline con release_year, runtime y director (random efects)

#data con generos duplicados
train_data_with_director <- train_data |>
  left_join(credits_train |> 
               filter(role == "DIRECTOR") |> 
               select(c("id", "name")) |>
               rename(director = "name"), by = "id") 

#ajuste
train <- train_data_with_director %>%
  dplyr::select(c("imdb_score", "release_year", "runtime", "director")) |>
  mutate(director = as.factor(director))

fit <- lmer(imdb_score ~ release_year + runtime + (1|director), data = train)

#data para testear
test_data<- test_data

test_data_with_director <- test_data |>
  left_join(credits_train |> 
               filter(role == "DIRECTOR") |> 
               select(c("id", "name")) |>
               rename(director = "name"), by = "id") 

test <- test_data_with_director %>%
  dplyr::select(c("id", "imdb_score", "release_year", "runtime", "director")) |>
  mutate(director = as.factor(director))

#predecimos
pred_director <- tibble(id=test$id)

pred <- predict(fit, newdata = test, allow.new.levels = TRUE)

pred_director <-pred_director %>%
  mutate(pred=pred)

error3 <- loss(pred_director$pred, test$imdb_score)

