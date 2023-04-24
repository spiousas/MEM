# Variedades de hongos ####
data_hongos <- read.delim("hongos_clasificados.txt", sep = " ") %>%
  mutate(Variety = factor(Variety))
head(data_hongos)

## 1 ####
data_hongos %>%
  ggplot(aes(x = Variety, y = Height, 
             fill = Variety)) +
  geom_boxplot() +
  theme_bw()

## 2 ####
data_hongos <- data_hongos %>%
  mutate(Variety_mod = as.numeric(Variety==2))
head(data_hongos)

## 3 ####
k <- 8

altura <- 5.2
prop_Variety <- sum(data_hongos$Variety_mod[order(abs(data_hongos$Height-altura))[1:k]]) / k
prop_Variety
cat("Se clasifica como de variedad", as.numeric(prop_Variety > 0.5) + 1)

altura <- 6
prop_Variety <- sum(data_hongos$Variety_mod[order(abs(data_hongos$Height-altura))[1:k]]) / k
prop_Variety
cat("Se clasifica como de variedad", as.numeric(prop_Variety > 0.5) + 1)

## 4 ####
h <- 0.1

altura <- 5.2
prop_Variety <- sum(data_hongos$Variety_mod[abs(data_hongos$Height-altura)<h]) / sum(abs(data_hongos$Height-altura)<h)
prop_Variety
cat("Se clasifica como de variedad", as.numeric(prop_Variety > 0.5) + 1)
cat("Se utilizan", sum(abs(data_hongos$Height-altura)<h), "alrededor de", altura, "puntos con h =", h)

altura <- 6
prop_Variety <- sum(data_hongos$Variety_mod[abs(data_hongos$Height-altura)<h]) / sum(abs(data_hongos$Height-altura)<h)
prop_Variety
cat("Se clasifica como de variedad", as.numeric(prop_Variety > 0.5) + 1)
cat("Se utilizan", sum(abs(data_hongos$Height-altura)<h), "alrededor de", altura, "puntos con h =", h)

# Alrededor de 5.2 hay más puntos que con NN, pero en 6 hay menos.
# Hay más concentración de hongos alrededor de 5.2 y lo podemos ver con un histograma

data_hongos %>%
  ggplot(aes(x = Height)) +
  geom_histogram() +
  theme_bw()

# 5 ####
class_knn <- function(x, y, x_nuevo, k) {
  as.numeric(sum(y[order(abs(x-x_nuevo))[1:k]])/k > 0.5)
}

class_knn(x = data_hongos$Height,
          y = data_hongos$Variety_mod,
          x_nuevo = 5.2,
          k = 8)

class_knn(x = data_hongos$Height,
          y = data_hongos$Variety_mod,
          x_nuevo = 6,
          k = 8)

class_prop_loc <- function(x, y, x_nuevo, h) {
  as.numeric(sum(y[abs(x-x_nuevo)<h]) / sum(abs(x-x_nuevo)<h) > 0.5)
}

class_prop_loc(x = data_hongos$Height,
               y = data_hongos$Variety_mod, 
               x_nuevo = 5.2,
               h = 0.1)

class_prop_loc(x = data_hongos$Height,
               y = data_hongos$Variety_mod, 
               x_nuevo = 6,
               h = 0.1)

## 6 ####
altura <- 5.2
h_opt_0 <- bw.ucv(data_hongos$Height[data_hongos$Variety_mod == 0])
P_x_0 <- density(data_hongos$Height[data_hongos$Variety_mod == 0], kernel = "gaussian", from = altura, to = altura, n =1, bw = h_opt_0)$y
prop_x_0 <- mean(data_hongos$Variety_mod == 0)

h_opt_1 <- bw.ucv(data_hongos$Height[data_hongos$Variety_mod == 1])
P_x_1 <- density(data_hongos$Height[data_hongos$Variety_mod == 1], kernel = "gaussian", from = altura, to = altura, n =1, bw = h_opt_1)$y
prop_x_1 <- mean(data_hongos$Variety_mod == 1)

cat("Se clasifica como de variedad", as.numeric(P_x_1 * prop_x_1 > P_x_0 * sum(data_hongos$Variety_mod == 0)) + 1)

altura <- 6
h_opt_0 <- bw.ucv(data_hongos$Height[data_hongos$Variety_mod == 0])
P_x_0 <- density(data_hongos$Height[data_hongos$Variety_mod == 0], kernel = "gaussian", from = altura, to = altura, n =1, bw = h_opt_0)$y
prop_x_0 <- mean(data_hongos$Variety_mod == 0)

h_opt_1 <- bw.ucv(data_hongos$Height[data_hongos$Variety_mod == 1])
P_x_1 <- density(data_hongos$Height[data_hongos$Variety_mod == 1], kernel = "gaussian", from = altura, to = altura, n =1, bw = h_opt_1)$y
prop_x_1 <- mean(data_hongos$Variety_mod == 1)

cat("Se clasifica como de variedad", as.numeric(P_x_1 * prop_x_1 > P_x_0 * prop_x_0) + 1)

## 7 ####
class_gen <- function(x, y, x_nuevo, h0, h1) {
  P_x_0 <- density(x[y == 0], kernel = "gaussian", from = x_nuevo, to = x_nuevo, n =1, bw = h0)$y
  prop_x_0 <- mean(y == 0)
  
  P_x_1 <- density(x[y == 1], kernel = "gaussian", from = x_nuevo, to = x_nuevo, n =1, bw = h1)$y
  prop_x_1 <- mean(y == 1)
  
  as.numeric(P_x_1 * prop_x_1 > P_x_0 * prop_x_0)
}

class_gen(x = data_hongos$Height,
          y = data_hongos$Variety_mod, 
          x_nuevo = 5.2,
          h0 = h_opt_0,
          h1 = h_opt_1)
