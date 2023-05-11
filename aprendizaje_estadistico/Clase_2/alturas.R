pacman::p_load(here, dplyr, tidyr, ggplot2)

# Ejercicio 1 ####
## 1 #### 
df_50 <- read.csv(here("aprendizaje_estadistico/Clase_2/data/alturas_n_50.csv"))
df_500 <- read.csv(here("aprendizaje_estadistico/Clase_2/data/alturas_n_500.csv"))

## 2 ####
names(df_50)

## 3 ####
mean(df_50$altura)
mean(df_500$altura)

## 4 ####
hist(df_50$altura, breaks = seq(min(df_50$altura), max(df_50$altura), length.out = 16))
hist(df_500$altura, breaks = seq(min(df_500$altura), max(df_500$altura), length.out = 16))

df_500 %>% ggplot(aes(x = altura, y = ..density..)) +
  geom_histogram(bins = 15, alpha = 0.3) +
  geom_density(color = "red") +
  theme_minimal()

## 5 ####
mean(df_50$altura[df_50$genero == "M"])
mean(df_50$altura)
mean(df_500$altura[df_500$genero == "M"])
mean(df_500$altura)

## 6 ####
unique(df_50$contextura_madre)

mean(df_50$altura[df_50$contextura_madre == "bajita"])
mean(df_50$altura[df_50$contextura_madre == "mediana"])
mean(df_50$altura[df_50$contextura_madre == "alta"])
mean(df_50$altura)
mean(df_500$altura[df_500$contextura_madre == "bajita"])
mean(df_500$altura[df_500$contextura_madre == "mediana"])
mean(df_500$altura[df_500$contextura_madre == "alta"])
mean(df_500$altura)

# Ejercicio 2 ####
## 7 ####
df_500 %>% ggplot(aes(x = altura_madre, y = altura, color = genero)) +
  geom_point() +
  theme_minimal()

alturasdat500m <- df_500 %>% filter(genero == "M")

## 8 ####
alturasdat500m %>% filter(altura_madre == 156)

## 9 ####
### a ####
# Con dplyr
alturasdat500m %>%
  filter(between(altura_madre, 155, 157)) %>%
  count()
# Con baseR
nrow(alturasdat500m[alturasdat500m$altura_madre>=155 & alturasdat500m$altura_madre<=157,])

### b ####
# Con dplyr
alturasdat500m %>%
  filter(between(altura_madre, 155, 157)) %>%
  summarise(pred = mean(altura))
# Con baseR
mean(alturasdat500m$altura[alturasdat500m$altura_madre>=155 & alturasdat500m$altura_madre<=157])

### c ####
# Con dplyr
alturasdat500m %>%
  filter(between(altura_madre, 154, 158)) %>%
  summarise(pred = mean(altura))
# Con baseR
mean(alturasdat500m$altura[alturasdat500m$altura_madre>=154 & alturasdat500m$altura_madre<=158])

# Ejercicio 3 ####
## 11 ####
# Con dplyr
# pred_prom_loc <- function(data, x_nueva, h) {
#   data %>%
#     filter(between(altura_madre, x_nueva-h, x_nueva+h)) %>%
#     summarise(pred = mean(altura)) %>%
#     pull(pred)
# }

# Con baseR
pred_prom_loc <- function(data, x_nueva, h) {
  mean(data$altura[data$altura_madre>=x_nueva-h & data$altura_madre<=x_nueva+h])
}

pred_prom_loc(alturasdat500m, 156, 2)

## 12 y 13 ####
# Con tidyr
predicciones <- expand_grid(h = c(1,2,3),
                            x_nueva = seq(151,168,length.out = 100)) %>%
  rowwise() %>%
  mutate(altura_pred = pred_prom_loc(data = alturasdat500m, x_nueva = x_nueva, h = h))

# Con ggplot2
predicciones %>%
  ggplot(aes(x = x_nueva, y = altura_pred, color = factor(h))) +
  geom_point() +
  geom_point(data = alturasdat500m, aes(x = altura_madre, y = altura), color = "black", alpha =.2) +
  theme_minimal()

# Ejercicio 4 ####
## 14 ####
data_500 <- read.csv(here("aprendizaje_estadistico/Clase_2/data/alturas_n_500.csv"))
alturasdat500m <- data_500 %>% filter(genero == "M")

y_pred <- ksmooth(x = alturasdat500m$altura_madre,
                  y = alturasdat500m$altura,
                  kernel = "box",
                  bandwidth = 4, # El ancho de banda para el Nadaraya-Watson con nucleo uniforme es 2h
                  x.points = 156)

y_pred$y

# Comparo con las predicciones de mi función
y_pred_old <- pred_prom_loc(alturasdat500m, 156, 2)
y_pred_old

predicciones <- expand_grid(x_nueva = seq(151,168,length.out = 100)) %>%
  rowwise() %>%
  mutate(altura_pred_propia = pred_prom_loc(data = alturasdat500m, x_nueva = x_nueva, h = 2),
         altura_pred_ksmooth =ksmooth(x = alturasdat500m$altura_madre,
                                      y = alturasdat500m$altura,
                                      kernel = "box",
                                      bandwidth = 4,
                                      x.points = x_nueva)$y)

predicciones %>%
  ggplot(aes(x = x_nueva, y = altura_pred_propia)) +
  geom_point(color = "red", size = 3) +
  geom_point(aes(y = altura_pred_ksmooth), color = "black") +
  theme_minimal()

## 15 ####
predicciones <- predicciones %>%
  rowwise() %>%
  mutate( altura_pred_ksmooth =ksmooth(x = alturasdat500m$altura_madre,
                                       y = alturasdat500m$altura,
                                       kernel = "box",
                                       bandwidth = 2,
                                       x.points = x_nueva)$y,
          altura_pred_norm =ksmooth(x = alturasdat500m$altura_madre,
                                   y = alturasdat500m$altura,
                                   kernel = "norm",
                                   bandwidth = 2,
                                   x.points = x_nueva)$y)

## 16 ####
predicciones %>%
  ggplot(aes(x = x_nueva, y = altura_pred_ksmooth)) +
  geom_point(data = alturasdat500m, aes(x = altura_madre, y = altura), color = "black", alpha = .2) +
  geom_line(color = "blue", alpha =.5) +
  geom_line(aes(y = altura_pred_norm), color = "red", alpha =.5) +
  labs(x = "Altura de la madre", y = "Altura") +
  theme_minimal()

# Ejercicio 5 ####
## 17 ####
alturasdat500m %>%
  mutate(dist = altura_madre-156) %>%
  arrange(abs(dist)) %>%
  slice_head(n = 7) %>%
  summarise(altura_pred = mean(altura)) %>%
  pull(altura_pred)

alturasdat500m$dist <- abs(alturasdat500m$altura_madre-156)
mean(alturasdat500m$altura[order(alturasdat500m$dist)[1:7]])

## 18 ####
knn.reg(train = alturasdat500m$altura_madre,
        test = 156,
        y = alturasdat500m$altura,
        k = 7)

# Ejercicio 6 ####
## 19 ####
pred_prom_loc <- function(x, y, x_nueva, h) {
  mean(y[abs(x-x_nueva)<=h])
}

### a ####
pred_1 <- pred_prom_loc(x = alturasdat500m$altura_madre[-1],
                        y = alturasdat500m$altura[-1],
                        x_nueva = alturasdat500m$altura_madre[1],
                        h = 1.5)

(alturasdat500m$altura[1] - pred_1)^2

### b ####
CV <- rep(NA, nrow(alturasdat500m))

for (i in (1:length(CV))) {
  pred_i <- pred_prom_loc(x = alturasdat500m$altura_madre[-i],
                          y = alturasdat500m$altura[-i],
                          x_nueva = alturasdat500m$altura_madre[i],
                          h = 1.5)
  
  CV[i] <- (alturasdat500m$altura[i] - pred_i)^2
}
CV[1]

### c ####
mean(CV)

### d ####
loocv <- function(h_i) {
  CV <- rep(NA, nrow(alturasdat500m))
  for (i in (1:length(CV))) {
    pred_i <- pred_prom_loc(x = alturasdat500m$altura_madre[-i],
                            y = alturasdat500m$altura[-i],
                            x_nueva = alturasdat500m$altura_madre[i],
                            h = h_i)
    
    CV[i] <- (alturasdat500m$altura[i] - pred_i)^2
  }
  mean(CV)
}

loocv(1.5)

### e ####
h_seq <- seq(1.5, 4, .05)
h_seq
CV_h <- rep(NA, length(h_seq))
for (j in 1:length(h_seq)) {
  CV_h[j] <- loocv(h_seq[j])
}

CV_h[1]

### f ####
h_seq[which.min(CV_h)]

## 20 ####
plot(h_seq, CV_h)  

## 21 ####
pred_prom_loc <- function(data, x_nueva, h) {
  mean(data$altura[data$altura_madre>=x_nueva-h & data$altura_madre<=x_nueva+h])
}

predicciones <- expand_grid(h = c(1,2,h_seq[which.min(CV_h)],5),x_nueva = seq(151,168,length.out = 100)) %>%
  rowwise() %>%
  mutate(altura_pred = pred_prom_loc(data = alturasdat500m, x_nueva = x_nueva, h = h))

predicciones %>%
  ggplot(aes(x = x_nueva, y = altura_pred, color = factor(h))) +
  geom_line() +
  geom_point(data = alturasdat500m, aes(x = altura_madre, y = altura), color = "black", alpha =.2) +
  theme_minimal()
