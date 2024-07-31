rm(list=ls())

library(tidyverse)
library(mgcv)

titles<-read.csv('titles_train.csv',sep=",")
credits<-read.csv('credits_train.csv',sep=",")

# me fijo si hay NA en algunas de las columnas que vamos a usar
sum(is.na(titles_train$title))
sum(is.na(titles_train$production_countries))
sum(is.na(titles_train$genres))
sum(is.na(titles_train$release_year))
sum(is.na(titles_train$imdb_score))

titles_train %>%
  select(c("title", "production_countries", "genres", "release_year", "imdb_score")) %>%  # replace to your needs
  summarise_all(list(~ sum(is.na(.))))

# miro paises (asi nomas)
levels(factor(titles_train$production_countries))
tabla<-table(factor(titles_train$production_countries))

titles_train_by_country <- titles_train |>
  separate_rows(production_countries, sep = ", ") |>
  filter(production_countries != "")  |>
  drop_na(imdb_score)

num_on_top <- 20
titles_train_by_country |>
  group_by(production_countries) |>
  summarise(N = n()) |>
  arrange(desc(N)) |>
  mutate(rank = row_number(),
         top_bottom = ifelse(rank <= num_on_top, "top", if_else(rank >= n()-num_on_top, "bottom", "otros"))) |>
  filter(top_bottom == "top") |>
  ggplot(aes(x = reorder(production_countries, N),
             y = N)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0("n=", N)), 
            color = "black", hjust = 0, size = 3) +
  labs(x = NULL, 
       y = "Rating promedio Rb") +
  coord_flip(expand = F) +
  theme_minimal() +
  theme(legend.position = "none")

titles_dummied <- titles_train_by_country |> 
  drop_na(imdb_score) |>
  select(c(id, production_countries, imdb_score)) |>
  dummy_cols(select_columns = "production_countries") |>
  select(-production_countries) |>
  group_by(id, imdb_score) |>
  summarise_all(sum) |>
  ungroup() |>
  select(-id)

fixed_dummy <- lm(data = titles_dummied, imdb_score ~ .)
fixed_full <- lm(data = titles_train_by_country, imdb_score ~ production_countries)

modelsummary(list(lm_dummy, lm_full))

random_countries <- lmer(data = titles_train_by_country, imdb_score ~ 0 + (1|production_countries))
min(predict(random_countries))

mixed_countries <- lmer(data = titles_train_by_country, imdb_score ~ production_countries + (1|production_countries))
min(predict(mixed_countries))
max(predict(mixed_countries))

# PUNTO 3

# sacamos las filas que tienen NA en imdb_score
titles_data<- titles  %>%
  filter(!is.na(imdb_score))

train_data <- train_data %>%
  dplyr::select(c("imdb_score", "release_year"))

# k=1 y 2 no deja ajustar, por algo de la dimension del espacio
Ks <- c(3, 5, 10, 20, 50)

pred_k <- tibble(release_year=min(train_data$release_year):max(train_data$release_year))

# para cada K ajustamos el modelo y predecimos en una grilla
for (K in Ks) {
  fit = gam(imdb_score ~ s(release_year, k = K , sp = 0, bs = 'cr'), data = train_data)
  pred = predict(fit, newdata = tibble(release_year = min(train_data$release_year):max(train_data$release_year)))
  pred_k <- pred_k %>%
    bind_cols(as_tibble(pred))
}


pred_k <- pred_k %>%
  set_names(c("release_year","3","5","10","20","50")) %>%
  pivot_longer(cols = -release_year, values_to = "imdb_score", names_to = "k")

# esto es porque me ponia desordenados los ks despues en el grafico
num_k<- as.numeric(pred_k$k)
pred_k <- pred_k %>% mutate(k= num_k)

# grafico de curvas estimadas para los distintos k
train_data %>% ggplot(aes(x=release_year,y=imdb_score)) +
  geom_point(color = "black", alpha = .2) +
  geom_line(data=pred_k, aes(x = release_year, y = imdb_score,color = as.factor(k)), linewidth = 1) +
  scale_color_brewer(palette = "Dark2")+
  labs(x = "Año", y = "Popularidad", color = "k")+
  theme_bw()

# otra cosa que se podria hacer (penalizacion)
fit=fit = gam(imdb_score ~ s(release_year, k=10, bs = 'cr'), data = train_data) 
# al no pasar sp, estima el sp (lambda) usando el que minimice GCV.

pred = predict(fit, newdata = tibble(release_year = min(train_data$release_year):max(train_data$release_year)))

tabla_pred<- tibble(release_year=min(train_data$release_year):max(train_data$release_year),imdb_score=pred)


train_data %>% ggplot(aes(x=release_year,y=imdb_score)) +
  geom_point(color = "darkgrey") +
  geom_line(data=tabla_pred, aes(x = release_year, y = imdb_score), linewidth = 1,color = "steelblue") +
  labs(x = "Año", y = "Popularidad", color = "k")+
  theme_bw() 

