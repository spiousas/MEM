pacman::p_load(ISLR2, tidyverse, tree, patchwork, randomForest, rsample)

# Problema 7 ####
ntrees <- seq(25,800,50)
mtrys <- seq(1,12,1)

head(Boston)

metricas <- tibble(ntree = numeric(),
                   mtry = numeric(),
                   MSE_test = numeric())

# Hago el split de train y test
set.seed(123)
split <- initial_split(data = Boston, strata = medv)
training_Boston <- training(split)
testing_Boston <- testing(split)

for(i in 1:length(mtrys)){
  for(j in 1:length(ntrees)){
    # Entreno con training
    rf <- randomForest(medv ~ ., 
                       data  = training_Boston, 
                       mtry  = mtrys[i], 
                       ntree = ntrees[j])
    # Predigo con testing
    yhat_rf <- predict(rf, 
                       newdata = testing_Boston)
    # Calculo el MSE
    mse <- mean(( yhat_rf - testing_Boston$medv)^2)
    metricas <- metricas %>% bind_rows(tibble(ntree = ntrees[j], mtry = mtrys[i], MSE_test = mse))
  }
}

metricas %>%
  ggplot(aes(x = ntree,
             color = as.factor(mtry),
             y = MSE_test)) +
  geom_point(size = 2) +
  geom_line(linewidth = 2, alpha = 0.5) +
  labs(color = "Mtry", y = "MSE de testeo") +
  scale_color_brewer(palette = "Paired") +
  theme_bw() +
  theme(legend.position = "bottom")

# Problema 8 ####
head(Carseats)

# Hago el split de train y test
set.seed(123)
split <- initial_split(data = Carseats, strata = Sales)
training_Carseats <- training(split)
testing_Carseats <- testing(split)

# Grafico el árbol
tree_carseats <- tree(formula = Sales ~ .,
                      data = training_Carseats)
plot(tree_carseats)
text(tree_carseats, pretty = 0)

# Calculo el error de testeo
MSE_test <- mean((predict(tree_carseats, newdata = testing_Carseats) - testing_Carseats %>% pull(Sales))^2)
MSE_test

# Hago el podado del árbol con CV
tree_carseats_cv <- cv.tree(tree_carseats)
plot(tree_carseats_cv)

tibble(size = tree_carseats_cv$size, deviance = tree_carseats_cv$dev) %>%
  ggplot(aes(x = size,
             y = deviance)) +
  geom_point(size = 2) +
  geom_line(alpha = .5, linewidth = 2) +
  labs(x = "Size", y = "Deviance", title = "Deviance en función del tamaño de árbol") +
  theme_bw()

best_size <- tree_carseats_cv$size[which.min(tree_carseats_cv$dev)]
best_size

tree_carseats_pruned <- prune.tree(tree_carseats, 
                                   best = best_size)
plot(tree_carseats_pruned)
text(tree_carseats_pruned, pretty = 0)
# Es el mismo y, por ende, el MSE de test va a ser igual

MSE_test_pruned <- mean((predict(tree_carseats_pruned, newdata = testing_Carseats) - testing_Carseats %>% pull(Sales))^2)
MSE_test_pruned

# Ahora uso bagging con la función random forest
# usamos bagging y estimamos el test mse
set.seed(123)
bagging_carseats <- randomForest(Sales ~ .,
                                 data = training_Carseats,
                                 mtry = ncol(training_Carseats) - 1,
                                 importance = TRUE,
                                 ntrees = 1000)
yhat.bag <- predict(bagging_carseats, newdata = testing_Carseats)

MSE_test_bagging <- mean((yhat.bag - testing_Carseats %>% pull(Sales))^2)
MSE_test_bagging

vip::vi(bagging_carseats) %>%
  ggplot(aes(x = Importance,
             y = forcats::fct_reorder(Variable, Importance))) +
  geom_col(fill = "steelblue") +
  labs(x = "Importancia", y = "Variable") +
  theme_minimal()

varImpPlot(bagging_carseats)
importance(bagging_carseats)

# Ahora hago random forest
mtrys <- seq(1,10,1)

metricas_rf <- tibble(ntree = numeric(),
                      mtry = numeric(),
                      MSE_test = numeric())

# Hago el split de train y test

for(i in 1:length(mtrys)){
  # Entreno con training
  set.seed(123)
  rf <- randomForest(Sales ~ .,
                     data = training_Carseats,
                     mtry = mtrys[i], 
                     importance = TRUE,
                     ntree = 1000)
  # Predigo con testing
  yhat_rf <- predict(rf, 
                     newdata = testing_Carseats)
  # Calculo el MSE
  mse <- mean(( yhat_rf - testing_Carseats$Sales)^2)
  metricas_rf <- metricas_rf %>% bind_rows(tibble(ntree = 1000, mtry = mtrys[i], MSE_test = mse))
}

metricas_rf %>%
  ggplot(aes(x = mtry,
             y = MSE_test)) +
  geom_point(size = 2) +
  geom_line(linewidth = 2, alpha = 0.5) +
  labs(x = "Mtry", y = "MSE de testeo") +
  scale_color_brewer(palette = "Paired") +
  theme_bw() 

