library(ISLR2)
library(tree)

# Ejercicio 2 ####
## 3 ####
str(Boston)

## 4 ####
set.seed(1)

sample <- sample(1:nrow(Boston), nrow(Boston)/2)
Boston_train <- Boston[sample,]
Boston_test <- Boston[-sample,]

tree_boston <- tree(medv~., Boston, subset = sample)
# o tree_boston <- tree(medv~., Boston_train)

## 5 ####
summary(tree_boston)

RSS <- sum((Boston_train$medv - predict(tree_boston, Boston_train))^2)
RSS

# Aparece en el summary en la:
#  Residual mean deviance:  10.38 = 2555 / 246 
# Donde 246 es la cantidad de datos (253) menos la cantidad de nodos terminales (7)

summary(tree_boston)$dev

## 6 ####
plot(tree_boston)
text(tree_boston, pretty = 1)

# El factor más importante es rm, que es el promedio de la cantidad de habitaciones (o sea, el primer split).

# Menor rm está asociado a menores precios medianos
# Menor lstat está asociado a mayores precios medianos

# A partir de la figura podemos ver que en la primera decisión va para la izquierda (rm<6.9595),
# En la segunda va de nuevo para la izquierda porque (lstat>14.405)
# Y, finalmente, en la tercera va de nuevo a la izquierda porque (rm<6.543).
# Entonces, el valor estimado por el árbol es de 21.38.

## 7 ####
# También lo podemos ver creando un x único con los datos que da en el TP.
data_i <- data.frame("crim" = 1.19294,
                     "zn" = 0,
                     "indus" = 21.89,
                     "chas" = 0,
                     "nox" = 0.624,
                     "rm" = 6.326,
                     "age" = 97.7,
                     "dis" = 2.271,
                     "rad" = 4,
                     "tax" = 437,
                     "ptratio" = 21.2,
                     "lstat" = 12.26)
data_i
predict(tree_boston, newdata = data_i)
# Y efectivamente eel valor predicho por el árbol es 21.38.

## 8 ####
MSEtrain <- mean((Boston_train$medv - predict(tree_boston, Boston_train))^2)
MSEtrain
MSEtest <- mean((Boston_test$medv - predict(tree_boston, Boston_test))^2)
MSEtest

# Uno es más grande que el otro porque minimice con los datos de entrenamiento

# Ejercicio 3 ####

## 9 ####
set.seed(3)

cv_boston <- cv.tree(tree_boston, K = 10)
cv_boston

## 10 ####
plot(cv_boston$size, cv_boston$dev, type = "b")

# Se alcanza el mínimo en el máximo tamaño posible, es decir, el árbol maximal es el óptimo

## 11 ####
prune5_boston <- prune.tree(tree_boston, best = 5)

plot(prune5_boston)
text(prune5_boston, pretty = 1)

# Se cortan las bifurcaciones de crim<11.4863 y agr<93.95

## 12 ####
MSEtest_full <- mean((Boston_test$medv - predict(tree_boston, Boston_test))^2)
MSEtest_full

MSEtest_pruned5 <- mean((Boston_test$medv - predict(prune5_boston, Boston_test))^2)
MSEtest_pruned5

# Tiene sentido que sean parecidos ya que los MSE de CV son bastante parecidos 
# entre el size 5 y size 7

## 13 ####
sqrt(MSEtest_full)

# El error promedio al estimar el precio mediano de las propiedades utilizando el árbol maximal
# es de $5940
