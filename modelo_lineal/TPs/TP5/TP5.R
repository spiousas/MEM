pacman::p_load(tidyverse, here)

# Ejercicio 1 ####
data_vinos <- read_csv(here("modelo_lineal/TPs/TP5/data/vinos.txt"))
data_vinos

## a ####
data_vinos %>%
  ggplot(aes(x = tiempo,
             y = precio)) +
  geom_point(alpha = .5) +
  theme_bw()

## b ####
model1_b <- data_vinos %>%
  lm (precio ~ tiempo, .)

## c ####
summary(model1_b)
#precio = 188.64 - 1.06*tiempo
summary(model1_b)$sigma

# Otra forma de calcular sigma es con los residuos (ver Ec 17, Pag 21, clase 3 y 4)
sqrt(t(summary(model1_b)$residuals) %*% summary(model1_b)$residuals / (nrow(data_vinos) - ncol(data_vinos)))

## c ####
data_vinos %>%
  ggplot(aes(x = tiempo,
             y = precio)) +
  geom_point(alpha = .5) +
  geom_abline(intercept = model1_b$coefficients[1], 
              slope = model1_b$coefficients[2],
              color = "red",
              linewidth = 2) +
  theme_bw()

## d ####
summary(model1_b)
# El coeficiente obtenido es sinificativo.
# El resultado obtenido no es razonable ya que el precio debería aumentar con los 
# meses de guarda de una botella

# Ejercicio 2 ####
data_vinos_2 <- read_csv(here("modelo_lineal/TPs/TP5/data/vinos2.txt")) %>%
  mutate(varie = factor(varie))
data_vinos_2

## a ####
data_vinos_2 %>%
  ggplot(aes(x = tiempo,
             y = precio,
             color = varie)) +
  geom_point(alpha = .5) +
  theme_bw()

## b ####
model2_b <- data_vinos_2 %>%
  lm (precio ~ tiempo + varie, .)
summary(model2_b)

## c ####
summary(model2_b)
#precio = 121 + 4.18*tiempo (si es varietal 1)
#precio = 60.2 + 4.18*tiempo (si es varietal 2)
#precio = -1.44 + 4.18*tiempo (si es varietal 3)
#precio = -70.9 + 4.18*tiempo (si es varietal 4)
summary(model2_b)$sigma

## d ####
# Todas las rectas tienen la misma pendiente y sólo difieren en la ordenada al origen
data_rectas <- tibble(
  varie = unique(data_vinos_2$varie),
  ordenadas = c(summary(model2_b)$coefficients[1,1], 
                summary(model2_b)$coefficients[1,1] + summary(model2_b)$coefficients[3:5,1]),
  pendientes = rep(summary(model2_b)$coefficients[2,1], 4))

## e ####
data_vinos_2 %>%
  ggplot(aes(x = tiempo,
             y = precio,
             color = varie)) +
  geom_point(alpha = .5) +
  geom_abline(data = data_rectas,
              aes(intercept = ordenadas, 
                  slope = pendientes,
                  color = varie),
              linewidth = 1) +
  theme_bw()

## f ####
# El coeficiente que acompaña a tiempo es significativo. Ahora sí tiene sentido,
# ya que, para cada varietal por separado hay una relación positiva entre precio
# y meses de guarda. Sin embargo, pareciera que la pendiente no refleja exactamente
# La relación entre las variables.

## g ####
# H0: beta_varie2 = beta_varie3 = beta_varie4 = 0
# vs.
# H1: Alguno de los betas es distinto de cero
anova(model1_b, model2_b)

# Plantenado todo como en el teorema 3.3
betas <- matrix(model2_b$coefficients)

# Quiero que los betas 3, 4 y 5 sean iguales a 0
A <- matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3)
A

C <- matrix(c(0, 0, 0), nrow = 3)
C

# La matriz del modelo
X <- model.matrix(model2_b)
X

# Calculo el F
F_val <- t(A %*% betas) %*% inv(A %*% inv(t(X) %*% X) %*% t(A)) %*% (A %*% betas) / (nrow(A)*summary(model2_b)$sigma^2)

# Y ahora el p-value
pf(df1 = nrow(A), df2 = nrow(X)-ncol(X), q = as.numeric(F_val), lower.tail = FALSE)

## h ####
# No estamos seguros, deberíamos probar si agregar una interacción (es decir,
# permitir que las pendientes sean diferentes para cada varietal) explica más 
# variabilidad.

# Ejercicio 3 ####
data_vinos_2 <- read_csv(here("modelo_lineal/TPs/TP5/data/vinos2.txt")) %>%
  mutate(varie = factor(varie))
data_vinos_2

## a ####
model3_a <- data_vinos_2 %>%
  lm (precio ~ tiempo * varie, .)
summary(model3_a)

## b ####
summary(model3_a)
# VARIETAL 1
#precio = 117.1712 + 4.4059*tiempo
# VARIETAL 2
#precio = (117.1712-55.3399) + (4.4059-0.2832)*tiempo
#precio = 61.8313 + 4.1227*tiempo
# VARIETAL 3
#precio = (117.1712-118.7989) + (4.4059-0.2159)*tiempo
#precio = -1.6277 + 4.19*tiempo
# VARIETAL 4
#precio = (117.1712-179.0754) + (4.4059-0.4172)*tiempo
#precio = -61.9042 + 3.9887*tiempo
summary(model3_a)$sigma

# Otra forma de calcular sigma es con los residuos (ver Ec 17, Pag 21, clase 3 y 4)
sqrt(t(summary(model3_a)$residuals) %*% summary(model3_a)$residuals / (nrow(data_vinos) - ncol(data_vinos)))

## b ####
data_rectas <- tibble(
  varie = unique(data_vinos_2$varie),
  ordenadas = c(summary(model3_a)$coefficients[1,1], 
                summary(model3_a)$coefficients[1,1] + summary(model3_a)$coefficients[3:5,1]),
  pendientes = c(summary(model3_a)$coefficients[2,1],
                 summary(model3_a)$coefficients[2,1] + summary(model3_a)$coefficients[6:8,1]))
data_rectas

data_vinos_2 %>%
  ggplot(aes(x = tiempo,
             y = precio,
             color = varie)) +
  geom_point(alpha = .5) +
  geom_abline(data = data_rectas,
              aes(intercept = ordenadas, 
                  slope = pendientes,
                  color = varie),
              linewidth = 1) +
  theme_bw()

## c ####
# H0: beta_tiempo:varie2 = beta_tiempo:varie3 = beta_tiempo:varie4 = 0
# vs.
# H1: Alguno de los betas es distinto de cero
anova(model2_b, model3_a)

# Plantenado todo como en el teorema 3.3
betas <- matrix(model3_a$coefficients)

# Quiero que los betas 3, 4 y 5 sean iguales a 0
A <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3)
A
C <- matrix(c(0, 0, 0), nrow = 3)
C

# La matriz del modelo
X <- model.matrix(model3_a)
X

# Calculo el F
F_val <- t(A %*% betas) %*% inv(A %*% inv(t(X) %*% X) %*% t(A)) %*% (A %*% betas) / (nrow(A)*summary(model3_a)$sigma^2)
F_val

# Y ahora el p-value
pf(df1 = nrow(A), df2 = nrow(X)-ncol(X), q = as.numeric(F_val), lower.tail = FALSE)

# No tenemos evidencia suficiente como para decir que los parámetros poblacionales de las 
# pendientes son diferentes.

