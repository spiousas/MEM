library(tree)
library(ISLR2)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rsample)
library(class)

# Problema 1 ####
Auto_mod <- Auto %>%
  mutate(mpg01 = as.factor(if_else(mpg<median(mpg), 0, 1)),
         origin_fac = case_when(
           origin == 1 ~ "American",
           origin == 2 ~ "European",
           origin == 3 ~ "Japanese",
           .default = as.character(origin)
         ))
  
Auto_mod %>%
  pivot_longer(mpg:origin, names_to = "var", values_to = "val") %>%
  ggplot(aes(x = as.factor(mpg01), y = val, color = var)) +
  geom_boxplot() + 
  geom_jitter(width = .2, alpha = .2) +
  labs(x = "mpg binarizado", y = NULL) +
  facet_wrap(.~ var, scales = "free_y") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(legend.position = "none")

Auto_mod %>%
  select(-mpg01) %>%
  pivot_longer(cylinders:origin, names_to = "var", values_to = "val") %>%
  ggplot(aes(x = mpg, y = val, color = var)) +
  geom_point(width = .2, alpha = .3) +
  geom_smooth(method = lm, se = FALSE, color = "gray50") +
  labs(x = "mpg", y = NULL) +
  facet_wrap(.~ var, scales = "free_y") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(legend.position = "none")

set.seed(1234)
split <- initial_split(data = Auto_mod %>% select(-origin), strata = mpg01)
training_Auto <- training(split)
testing_Auto <- testing(split)

m0 <- glm(data = training_Auto %>% select(-c(name, mpg)), mpg01 ~ 1, family = "binomial")
summary(m0)
mp <- formula(glm(data = training_Auto %>% select(-c(name, mpg)), mpg01 ~ ., family = "binomial"))
summary(mp)
m1 <- step(m0, scope = mp, direction = "forward")

testing_Auto <- 
  testing_Auto %>%
  mutate(pred = as.factor(if_else(predict.glm(m1, 
                                              newdata = testing_Auto %>% select(c(displacement, year, weight, origin_fac, horsepower)), 
                                              type = "response") > 0.5, 1, 0)))

1 - sum(testing_Auto$mpg01 == testing_Auto$pred) / nrow(testing_Auto)

# KNN
set.seed(123)
split_scaled <- Auto_mod %>% 
  select(-c(name, mpg, mpg01, origin, origin_fac)) %>%
  mutate(across(everything(), scale)) %>%
  mutate(mpg01 = Auto_mod$mpg01) %>%
  initial_split(data = ., strata = mpg01)
split_scaled

training_scaled <- training(split_scaled)
testing_scaled  <- testing(split_scaled)

Xtrain <- training_scaled %>% select(-mpg01)
Ytrain <- training_scaled %>% pull(mpg01)

Xtest <- testing_scaled %>% select(-mpg01)
Ytest <- testing_scaled %>% pull(mpg01)

Ks <- 1:40
err <- rep(NA, length(Ks))
for (i in Ks) {
  set.seed(123)
  pred = knn(train = Xtrain, 
             test = Xtest, 
             cl = Ytrain, 
             k = i, 
             prob = TRUE)
  
  err[i] <- 1 - mean(pred == Ytest)
}

tibble(Ks, err) %>%
  ggplot(aes(x = Ks,
             y = err)) +
  geom_point(size = 2) +
  geom_line(alpha = .5, linewidth = 2) +
  labs(x = "K", y = "Error de clasificación", title = "Error de clasificación en función de K para KNN") +
  theme_bw()

# Problema 3 ####
## a ####
set.seed(12)
split <- initial_split(data = OJ, prop = 800/nrow(OJ), strata = Purchase)
training_OJ <- training(split)
testing_OJ <- testing(split)

## b ####
tree_purchase <- tree(Purchase ~ ., data = training_OJ)

summary(tree_purchase)
# Tiene 7 nodos terminales
# El error de entrenamiento es 0.165

## c ####
tree_purchase
# Si loyarCH es menor que 0.48285 y loyalCH es menor que 0.05 entonces purchase se clasifica como MM
# Lo segundo que aparece es la deviance (pagina 353 del ISLR) que es una suma en las clases
# por el p de cada clase -2 * (488 * log(0.61076) + 311 * log(1-0.61076))

## d ####
plot(tree_purchase)
text(tree_purchase, pretty = 0)

## e ####
pred_test <- predict(tree_purchase, newdata = testing_OJ, type = "class")
(t <- table(pred_test, testing_OJ$Purchase))
confusionMatrix(pred_test, testing_OJ$Purchase)

## f ####
tree_purchase_cv <- cv.tree(tree_purchase, K = 5)
plot(tree_purchase_cv)

tibble(size = tree_purchase_cv$size, deviance = tree_purchase_cv$dev) %>%
  ggplot(aes(x = size,
             y = deviance)) +
  geom_point(size = 2) +
  geom_line(alpha = .5, linewidth = 2) +
  labs(x = "Size", y = "Deviance", title = "Deviance en función del tamaño de árbol") +
  theme_bw()
# Da la mínima deviance para size = 6  

## g ####
tree_purchase_pruned <- prune.tree(tree_purchase, best = tree_purchase_cv$size[which.min(tree_purchase_cv$dev)] )
tree_purchase_pruned

plot(tree_purchase_pruned)
text(tree_purchase_pruned, pretty = 0)

## h ####
# Comparo los errores de entrenamiento
cat(paste("Error de entrenamiento del arbol completo", 
          round(summary(tree_purchase)$misclass[1]/summary(tree_purchase)$misclass[2], digits = 3)))
cat(paste("Error de entrenamiento del arbol podado", 
          round(summary(tree_purchase_pruned)$misclass[1]/summary(tree_purchase_pruned)$misclass[2], digits = 3)))
# El error de entrenamiento es más grande en el podado

## i ####
pred_test <- predict(tree_purchase, newdata = testing_OJ, type = "class")
cat(paste("Error de testeo del arbol completo", round(1 - mean(pred_test == testing_OJ$Purchase), digits = 3)))

pred_test_pruned <- predict(tree_purchase_pruned, newdata = testing_OJ, type = "class")
cat(paste("Error de testeo del arbol podado", round(1 - mean(pred_test_pruned == testing_OJ$Purchase), digits = 3)))
# El error de testeo es más grande en el podado