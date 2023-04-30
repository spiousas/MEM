# Clasificador K vecinos cercanos ####
class_knn <- function(x, y, x_nuevo, k) {
  as.numeric(sum(y[order(abs(x-x_nuevo))[1:k]])/k > 0.5)
}

# Clasificador de proporciones locales ####
class_prop_loc <- function(x, y, x_nuevo, h) {
  as.numeric(sum(y[abs(x-x_nuevo)<h]) / sum(abs(x-x_nuevo)<h) > 0.5)
}

# Clasificador generativo ####
class_gen <- function(x, y, x_nuevo, h0, h1) {
  P_x_0 <- density(x[y == 0], kernel = "gaussian", from = x_nuevo, to = x_nuevo, n =1, bw = h0)$y
  prop_x_0 <- mean(y == 0)
  
  P_x_1 <- density(x[y == 1], kernel = "gaussian", from = x_nuevo, to = x_nuevo, n =1, bw = h1)$y
  prop_x_1 <- mean(y == 1)
  
  as.numeric(P_x_1 * prop_x_1 > P_x_0 * prop_x_0)
}

