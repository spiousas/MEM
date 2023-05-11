library(tree)
library(dplyr)
library(tidyr)
library(here)

# Ejercicio 1 ####
## 1 ####
data <- airquality %>% drop_na(c(Ozone, Wind))

## 2 ####
data_20 <- data[1:20,]
data_20

tree_air_20 <- tree(Ozone~Wind, data_20)
summary(tree_air_20)

plot(tree_air_20)
text(tree_air_20, pretty = 1)

## 3 ####
data_20 <- data_20 %>% 
  arrange(Wind)

### a ####
RSS <- c()
seq_bound <- 5:15
for (i in 1:length(seq_bound)) {
  RSS_R1 <- sum((data_20$Ozone[1:seq_bound[i]] - mean(data_20$Ozone[1:seq_bound[i]]))^2)
  RSS_R2 <- sum((data_20$Ozone[(seq_bound[i]+1):20] - mean(data_20$Ozone[(seq_bound[i]+1):20]))^2)
  print(data_20$Ozone[seq_bound[i]+1:20])
  RSS[i] <- RSS_R1 + RSS_R2
}

### b ####
plot(seq_bound, RSS)
seq_bound[which.min(RSS)]

# Me da que el RSS más chico es dejando 5 y 15
cat("El minimo del RSS es en Wind=", data_20$Wind[seq_bound[which.min(RSS)]])

# Pero corta entre el promedio entre ese punto y el que viene
cat("El arbol corta en Wind=", mean(c(data_20$Wind[seq_bound[which.min(RSS)]], data_20$Wind[seq_bound[which.min(RSS)+1]])))

### c ####
# El RSS
sum((data_20$Ozone - predict(tree_air_20, data_20))^2)
summary(tree_air_20)

# Ejercicio 2 ####
## 1 ####
set.seed(1)
sample <- sample(1:nrow(data), 76)
data_train <- airquality[sample,] # Vuelvo a airquality original
data_test <- airquality[-sample,]

## 2 ####
tree_air <- tree(Ozone~., data_train)

plot(tree_air)
text(tree_air, pretty = 1)

summary(tree_air)
# Tiene 8 nodos terminales y fueron utilizadas 4 variables
# El RSS del arbol es 16570
y <- data_train %>% drop_na() %>% pull(Ozone)
x <- data_train %>% drop_na()
sum((y - predict(tree_air, x))^2)

# El MSE de entrenamiento es RSS/n (que en este caso es 48)
MSEtrain <- sum((y - predict(tree_air, x))^2) / length(y)
MSEtrain

# La variable mas importante es Temp (el primer split)

## 3 ####
y[4] - predict(tree_air, x[4,])

res <- c()
for (i in 1:length(y)) {
  res[i] <- y[i] - predict(tree_air, x[i,])   
}

mean(res)
median(res)
summary(tree_air)

## 4 ####
data_i <- data.frame("Solar.R" = 120,
                     "Wind" = 10.5,
                     "Temp" = 62,
                     "Day" = 7,
                     "Month" = 6)
data_i
predict(tree_air, newdata = data_i)

## 5 ####
y <- data_test %>% drop_na() %>% pull(Ozone)
x <- data_test %>% drop_na()
MSEtest <- mean((y - predict(tree_air, x))^2)
MSEtest
# Como es de esperarse, da bastante mas grande que el de train

# Ejercicio 3 ####
zarig <- read.csv(here("aprendizaje_estadistico/Pr_6/data/zariguella.csv")) %>%
  select(-X)
zarig

## 1 ####
set.seed(2)

sample <- sample(1:nrow(zarig), nrow(zarig)/2)
zarig_train <- zarig[sample,]
zarig_test <- zarig[-sample,]

tree_zarig <- tree(age~., zarig_train)

plot(tree_zarig)
text(tree_zarig, pretty = 1)
summary(tree_zarig)

## 2 ####
set.seed(3)
cv_zarig <- cv.tree(tree_zarig, K = 10)
cv_zarig
plot(cv_zarig$size, cv_zarig$dev, type = "b")

## 3 ####
Toptim <- cv_zarig$size[which.min(cv_zarig$dev)]
Toptim
prune_optim_zarig <- prune.tree(tree_zarig, best = Toptim)

plot(prune_optim_zarig)
text(prune_optim_zarig, pretty = 1)
summary(prune_optim_zarig)

## 4 ####
MSEtest_full <- mean((zarig_test$age - predict(tree_zarig, zarig_test))^2)
MSEtest_full

MSEtest_pruned5 <- mean((zarig_test$age - predict(prune_optim_zarig, zarig_test))^2)
MSEtest_pruned5

sqrt(MSEtest_pruned5)
# El error promedio al estimar la edad de la zariguella utilizando el arbol podado es de 2,16 anios