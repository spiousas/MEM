library(tidyverse)
library(here)
library(FNN)

lidar <- read_delim(here("aprendizaje_estadistico/Pr_2/data/lidar.txt"), delim = ' ') %>%
  rename('logratio' = 'int.conc')
lidar %>% head()

# Ejercicio 1 ####
p1 <- lidar %>% 
  ggplot(aes(x = range,
             y = logratio)) +
  geom_point(alpha = .5) +
  theme_bw()
p1

# Aproximadamente hasta el valor de range = 550 no hay una relación entre ambas variables,
# pero a partir de ahí comienza a haber un decrecimiento de logratio al aumentar range.
# Después de los 600 la relación se vuelve mucho más ruidosa (más variabilidad).

# Ejercicio 2 ####
pred_prom_loc <- function(x, y, x_nueva, h) {
  mean(y[abs(x-x_nueva)<=h])
}

pred_prom_loc(x = lidar$range, 
              y = lidar$logratio,
              x_nueva = 602,
              h = 5)

p2 <- p1 +
  geom_point(x = 602, y = pred_prom_loc(x = lidar$range, 
                                        y = lidar$logratio, 
                                        x_nueva = 602, 
                                        h = 5), 
             color = "red", 
             size = 3) 
p2

# Ejercicio 3 ####
# Solo con h=5
lidar_pred <-tibble(range = seq(min(lidar$range), max(lidar$range))) %>%
  rowwise() %>%
  mutate(logratio_pred = pred_prom_loc(x = lidar$range, 
                                       y = lidar$logratio,
                                       x_nueva = range,
                                       h = 5))
p3 <- p2 + 
  geom_line(data = lidar_pred, aes(x = range, y = logratio_pred), color = "blue", size = 1)
p3

# Con h=5, h=10 y h=30
lidar_pred <- expand_grid(range = seq(min(lidar$range), max(lidar$range)),
                          h = c(5, 10, 30)) %>%
  rowwise() %>%
  mutate(logratio_pred = pred_prom_loc(x = lidar$range, 
                                       y = lidar$logratio,
                                       x_nueva = range,
                                       h = h))

p3 <- p2 + 
  geom_line(data = lidar_pred, aes(x = range, y = logratio_pred, color = factor(h)), size = 1) +
  theme(legend.position = "top")
p3

# Ejercicio 4 ####
# Ahora hago la predicción con ksmooth, con un kernel normal y ancho de ventana h=5
lidar_pred_ks <-tibble(range = seq(min(lidar$range), max(lidar$range))) %>%
  rowwise() %>%
  mutate(logratio_pred = ksmooth(x = lidar$range, 
                                 y = lidar$logratio,
                                 kernel = "norm",
                                 bandwidth = 5,
                                 x.points = range)$y)

p4 <- p1 + 
  geom_line(data = lidar_pred_ks, aes(x = range, y = logratio_pred), color = "blue", size = 1)
p4

# Ejercicio 5 ####
# Repito el ejercicio 4 pero para h=10, h=30 y h=50
lidar_pred_ks <-expand_grid(range = seq(min(lidar$range), max(lidar$range)),
                            h = c(5, 10, 30, 50)) %>%
  rowwise() %>%
  mutate(logratio_pred = ksmooth(x = lidar$range, 
                                 y = lidar$logratio,
                                 kernel = "norm",
                                 bandwidth = h,
                                 x.points = range)$y)

p5 <- p1 + 
  geom_line(data = lidar_pred_ks, aes(x = range, y = logratio_pred, color = factor(h)) , size = 1) +
  theme(legend.position = "top")
p5

# Como es de esperarse, cuanto más grande la ventana más "suavizada" la estimación

# Ejercicio 6 ####
# Ahora vamos a calcular los MSE para los distintos valores de h (entregable)
MSE_ks <-expand_grid(range = lidar$range,
                     h = c(5, 10, 30, 50)) %>%
  left_join(lidar, by = "range") %>%
  rowwise() %>%
  mutate(logratio_pred = ksmooth(x = lidar$range, 
                                 y = lidar$logratio,
                                 kernel = "norm",
                                 bandwidth = h,
                                 x.points = range)$y,
         SqErr = (logratio - logratio_pred)^2) %>%
  group_by(h) %>%
  summarise(MSE = sum(SqErr)/n())

MSE_ks

# Ejercicio 7 ####
# Ahora vamos a buscar el h óptimo usando el algoritmo de LOOCV

loocv <- function(lidar, h_i) {
  CV <- rep(NA, nrow(lidar))
  for (i in (1:length(CV))) {
    pred_i <- ksmooth(x = lidar$range[-i], 
                      y = lidar$logratio[-i],
                      kernel = "norm",
                      bandwidth = h_i,
                      x.points = lidar$range[i])$y
    
    CV[i] <- (lidar$logratio[i] - pred_i)^2
  }
  mean(CV)
}

CV_h <- tibble(h = seq(3, 165)) %>%
  rowwise() %>%
  mutate(CV =  loocv(lidar, h))

cat(paste("El h óptimo es:", CV_h$h[which.min(CV_h$CV)]))

CV_h %>% ggplot(aes(x = h,
           y = CV)) +
  geom_point(alpha = .2) +
  geom_line() +
  geom_vline(xintercept = CV_h$h[which.min(CV_h$CV)], color = "red", linetype = "dashed") +
  geom_hline(yintercept = CV_h$CV[which.min(CV_h$CV)], color = "red", linetype = "dashed") + 
  geom_label(x = CV_h$h[which.min(CV_h$CV)],
            y = CV_h$CV[which.min(CV_h$CV)],
            label = paste0("h= ", CV_h$h[which.min(CV_h$CV)], " - y= ", round(CV_h$CV[which.min(CV_h$CV)], digits = 2)), 
            hjust = 0,
            vjust = -5) +
  theme_bw()

# Ejercicio 8 ####

CV_hopt <- CV_h$CV[which.min(CV_h$CV)]
CV_hopt

MSE_hopt <- lidar %>%
  rowwise() %>%
  mutate(logratio_pred = ksmooth(x = lidar$range, 
                                 y = lidar$logratio,
                                 kernel = "norm",
                                 bandwidth = CV_h$h[which.min(CV_h$CV)],
                                 x.points = range)$y,
         SqErr = (logratio - logratio_pred)^2) %>%
  ungroup() %>%
  summarise(MSE = sum(SqErr)/n())
MSE_hopt$MSE

# Es más grande el CV porque el MSE es el ajuste con todos los puntos.

# Ejercicio 9 ####
lidar_pred_ks_optim <-tibble(range = lidar$range) %>%
  rowwise() %>%
  mutate(logratio_pred = ksmooth(x = lidar$range, 
                                 y = lidar$logratio,
                                 kernel = "norm",
                                 bandwidth = CV_h$h[which.min(CV_h$CV)],
                                 x.points = range)$y)

p9 <- p1 + 
  geom_line(data = lidar_pred_ks_optim, aes(x = range, y = logratio_pred), color = "blue" , size = 1) +
  theme(legend.position = "top")
p9

# Ejercicio 10 ####
# Agreguemos la predicción para el rango=570 con pred_prom_loc como un punto rojo 
p10 <- p9 +
  geom_point(x = 570, y = pred_prom_loc(x = lidar$range, 
                                        y = lidar$logratio, 
                                        x_nueva = 570, 
                                        h = 5), 
             color = "red", 
             size = 2) 
p10

# Ejercicio 11 ####
# Ahora creamos una función para hacer la estimación con knn y la ploteamos para k=30
pred_knn <- function(x, y, x_nuevo, k) {
  mean(y[order(abs(x-x_nuevo))[1:k]])
}

lidar_pred_knn <-tibble(range = lidar$range) %>%
  rowwise() %>%
  mutate(logratio_pred = pred_knn(x = lidar$range, 
                                  y = lidar$logratio,
                                  x_nuevo = range,
                                  k = 30))

p11 <- p1 + 
  geom_line(data = lidar_pred_knn, aes(x = range, y = logratio_pred), color = "blue" , size = 1) +
  theme(legend.position = "top")
p11

# Ejercicio 12 ####
preds_knn <- tibble(range = rep(570, 3), 
                    k = c(5, 20, 40)) %>%
  rowwise() %>%
  mutate(logratio_pred_knn = pred_knn(x = lidar$range, 
                                      y = lidar$logratio,
                                      x_nuevo = range,
                                      k = k))

preds_knn

# Ejercicio 12 ####
x_train <- as.data.frame(lidar["range"])
y_train <- as.data.frame(lidar["logratio"])
k <- 40
test <- data_frame(range = 570)

knn.reg(train = x_train,
        test = test,
        y = y_train,
        k = 10)

loocv_knn <- function(lidar, k_i) {
  lidar <- as.data.frame(lidar)
  CV <- rep(NA, nrow(lidar))
  for (i in (1:length(CV))) {
    pred_i <- knn.reg(train = lidar["range"][-i,], 
                      y = lidar["logratio"][-i,],
                      test = lidar["range"][i,],
                      k = k_i)$pred
    
    CV[i] <- (lidar$logratio[i] - pred_i)^2
  }
  mean(CV)
}

# Lo testeo con 1
loocv_knn(lidar, 40)

CV_k <- tibble(k = seq(1, 50)) %>%
  rowwise() %>%
  mutate(CV =  loocv_knn(lidar, k))

cat(paste("El h óptimo es:", CV_k$k[which.min(CV_k$CV)]))

CV_k %>% ggplot(aes(x = k,
                    y = CV)) +
  geom_point(alpha = .2) +
  geom_line() +
  geom_vline(xintercept = CV_k$k[which.min(CV_k$CV)], color = "red", linetype = "dashed") +
  geom_hline(yintercept = CV_k$CV[which.min(CV_k$CV)], color = "red", linetype = "dashed") + 
  geom_label(x = CV_k$k[which.min(CV_k$CV)],
             y = CV_k$CV[which.min(CV_k$CV)],
             label = paste0("h= ", CV_k$k[which.min(CV_k$CV)], " - y= ", round(CV_k$CV[which.min(CV_k$CV)], digits = 2)), 
             hjust = 0,
             vjust = -5) +
  theme_bw()

# Ahora con el k óptimo vuelvo a calcular el ajuste y el MSE
CV_kopt <- CV_k$CV[which.min(CV_k$CV)]
CV_kopt

MSE_kopt <- lidar %>%
  rowwise() %>%
  mutate(logratio_pred = knn.reg(train = lidar["range"], 
                                 y = lidar["logratio"],
                                 test = data.frame(range = range),
                                 k = CV_k$k[which.min(CV_k$CV)])$pred,
         SqErr = (logratio - logratio_pred)^2) %>%
  ungroup() %>%
  summarise(MSE = sum(SqErr)/n())
MSE_kopt$MSE
# De nuevo el MSE con todos los datos es más chico que el CV

# Por último vamos a plotear la predicción
lidar_pred_knn_optim <- lidar %>%
  dplyr::select(range) %>%
  rowwise() %>%
  mutate(logratio_pred = knn.reg(train = lidar["range"], 
                                 y = lidar["logratio"],
                                 test = data.frame(range = range),
                                 k = CV_k$k[which.min(CV_k$CV)])$pred)

p13 <- p1 + 
  geom_line(data = lidar_pred_knn_optim, aes(x = range, y = logratio_pred), color = "blue" , size = 1) +
  theme(legend.position = "top")
p13