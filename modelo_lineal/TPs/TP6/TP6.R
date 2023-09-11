pacman::p_load(tidyverse, here, AER)
colormap <- c("#E69F00", "#56B4E9", "#51513D", "#A23B72", "#88292F")

# Ejercicio 1 ####
data(CASchools)
CASchools <- tibble(CASchools)
CASchools

## a ####
fig_simple <- 
  CASchools %>% ggplot(aes(x = income,
                         y = math)) +
  geom_point(alpha = .5) + 
  theme_bw()
fig_simple
  
## b ####
model_grado1 <- lm(data = CASchools, math ~ income)

## c ####
model_grado1
# El modelo es math = 625.539 + 1.815 * income
sigma <- sqrt(sigma(model_grado1))
sigma

## d ####
fig_grado1 <- fig_simple + 
  geom_abline(intercept = model_grado1$coefficients[1], slope = model_grado1$coefficients[2],
              color = colormap[1], linewidth = 1.5)
fig_grado1

## e ####
summary(model_grado1)
# El beta es significativo y el modelo, en principio, tiene sentido, a más 
# income en la casa mejores notas en math

## f ####
CASchools %>% select(income) %>%
  mutate(residuos = summary(model_grado1)$residuals) %>%
  ggplot(aes(x = income,
             y = residuos)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = .5) + 
  theme_bw()

# Los residuos deberían tener la mima variabilidad (el mismo ancho de la nube)
# para todos los valores de income y no es así

## g ####
CASchools <- CASchools %>%
  mutate(income_2 = income^2)
model_grado2 <- lm(data = CASchools, math ~ income + income_2)
model_grado2

# El modelo es math = 610.34645 + 3.47255 * income - 0.03555 * income^2

## h ####
fig_grado2 <- fig_grado1 + 
  geom_line(aes(y = model_grado2$fitted.values),
            color = colormap[2], linewidth = 1.5)
fig_grado2
            
## i ####
summary(model_grado2)
# El beta es significativo y el modelo, en principio, tiene sentido, a más 
# income en la casa mejores notas en math pero esto parece plancharse luego de 
# income lo suficientemente grande

## j ####
CASchools <- CASchools %>%
  mutate(income_3 = income^3)
model_grado3 <- lm(data = CASchools, math ~ income + income_2 + income_3)
model_grado3

fig_grado3 <- fig_grado2 + 
  geom_line(aes(y = model_grado3$fitted.values),
            color = colormap[3], linewidth = 1.5)
fig_grado3

summary(model_grado3)
# El beta del término cúbico no es significativo y lo que es más curioso aún
# es que el término del cuadrático ahora tampoco lo es.

## k####
# Quiero que los betas 2 y 3
A <- matrix(c(0, 0, 0, 0, 1, 0, 0, 1), nrow = 2)
A
C <- matrix(c(0, 0), nrow = 2)
C

# La matriz del modelo
X <- model.matrix(model_grado3)
X

# Los betas
betas <- model_grado3$coefficients

# Calculo el F
F_val <- t(A %*% betas) %*% solve(A %*% solve(t(X) %*% X) %*% t(A)) %*% (A %*% betas) / (nrow(A)*summary(model_grado3)$sigma^2)
F_val

n <- nrow(X)
p <- ncol(X)
pf(df1 = n, df2 = n-p, q = as.numeric(F_val), lower.tail = FALSE)

# Con anova
anova(model_grado1, model_grado3)

## m ####
sigma(model_grado1)
sigma(model_grado2)
sigma(model_grado3)

## ñ ####
CASchools <- CASchools %>%
  mutate(log_income = log(income))
model_log <- lm(data = CASchools, math ~ log_income)
model_log
# La relación me parece razonable pero no me parece fácilmente interpretable

## o ####
fig_log <- fig_grado3 + 
  geom_line(aes(y = model_log$fitted.values),
            color = colormap[4], linewidth = 1.5)
fig_log

# Ejercicio 2 ####
data(CASchools)
CASchools <- tibble(CASchools) %>%
  mutate(stratio = students/teachers,
         score = (math + read)/2)
CASchools

## a ####
CASchools %>% ggplot(aes(x = math,
           y = read)) +
  geom_point(alpha = .4) +
  geom_smooth(method = lm, se = FALSE) +
  theme_bw()

cor(CASchools$math, CASchools$read)
  
## b ####
model_stratio <- lm(data = CASchools,
                    score ~ stratio)
model_stratio

CASchools %>% ggplot(aes(x = stratio,
                         y = score)) +
  geom_point(alpha = .4) +
  geom_abline(intercept = model_stratio$coefficients[1], slope = model_stratio$coefficients[2],
              color = colormap[1], linewidth = 1.5) +
  theme_bw()

summary(model_stratio)
# La variable es significativa y su vínculo es razonable. En cuanto a si proporciona 
# una buena explicación de las notas es discutible ya que al subir la cantidad
# de estudiantes por docente de, por ejemplo, 14 a 24, sólo se explica una subida
# de 22.8 en la nota.

## c.I ####
CASchools <- tibble(CASchools) %>%
  mutate(low_no_eng = factor(if_else(english>8, 0, 1)))
# Ese nuevo facto vale 1 cuando 8% o menos de los estudiantes NO tiene inglés como
# idioma nativo

## c.II ####
CASchools %>% ggplot(aes(x = stratio,
                         y = score,
                         color = low_no_eng)) +
  geom_point(alpha = .4) +
  scale_color_manual(values = colormap) +
  theme_bw() +
  theme(legend.position = "top")

# Se observa que para los que tienen mas de 8% de estudiantes sin inglés nativo
# hay una relación más importante entre score y stratio mientras que para el ç
# otro grupo pareciera no haber dependencia.

## c.III ####
model_stratio_eng <- lm(data = CASchools,
                        score ~ stratio * low_no_eng)
summary(model_stratio_eng)

# El modelo es score = 680.5047 - 1.7968 * stratio # Para escales con ingles>8%
# El modelo es score = (680.5047 + 2.5678) + (-1.7968 + 0.8311) * stratio # Para escales con ingles<=8%
#              score = 683.0725 - 0.9657 * stratio

# El aumento de 1 unidad en stratio hace disminuir el score en 1.8 en ingles>8%
# y en 0.97 en inglés<=8%. O sea, es más importante mejorar el ratio de alumnos
# por docente cuando hay más proporción de alumnos sin inglés nativo.

# Los coeficientes no son significativos

## c.IV ####
model_stratio_eng_no_int <- lm(data = CASchools, 
                               score ~ stratio + low_no_eng)
summary(model_stratio_eng_no_int)

CASchools %>% ggplot(aes(x = stratio,
                         y = score,
                         color = low_no_eng)) +
  geom_point(alpha = .4) +
  scale_color_manual(values = colormap) +
  geom_abline(intercept = model_stratio_eng_no_int$coefficients[1], slope = model_stratio_eng_no_int$coefficients[2],
              color = colormap[1], linewidth = 1) +
  geom_abline(intercept = model_stratio_eng_no_int$coefficients[1] + model_stratio_eng_no_int$coefficients[3], 
              slope = model_stratio_eng_no_int$coefficients[2],
              color = colormap[2], linewidth = 1) +
  theme_bw() +
  theme(legend.position = "top")

# El ajuste tiene sentido, pero al no tener interacción ambas pendientes son iguales
# Muestra que en general cuando hay muchos alumnos sin inglés nativo las notas son más
# bajas en general (acá hay olor a covariables).

## d.I ####
CASchools <- tibble(CASchools) %>%
  mutate(classeng = case_when(
    english <= 5 ~ 1,
    (english > 5) &  (english <= 10) ~ 2,
    (english > 10) &  (english <= 20) ~ 3,
    (english > 20) &  (english <= 50) ~ 4,
    english > 50 ~ 5),
    classeng = factor(classeng))

## d.II ####
CASchools %>% ggplot(aes(x = classeng,
           y = score,
           color = classeng)) +
  geom_boxplot() +
  scale_color_manual(values = colormap) +
  theme_bw() +
  theme(legend.position = "top")

## d.III ####
CASchools %>% ggplot(aes(x = stratio,
                         y = score,
                         color = classeng)) +
  geom_point(alpha = .5) +
  scale_color_manual(values = colormap) +
  theme_bw() +
  theme(legend.position = "top")

## d.IV ####
model_stratio_classeng <- lm(data = CASchools, 
                             score ~ stratio * classeng)
summary(model_stratio_classeng)

# H0: stratio:classeng2 = stratio:classeng3 = stratio:classeng4 = stratio:classeng5 = 0
# vs.
# H1: Alguno de los betas es distinto de cero
model_stratio_classeng_no_interaction <- lm(data = CASchools, 
                                            score ~ stratio + classeng)
anova(model_stratio_classeng_no_interaction, model_stratio_classeng)

# Plantenado todo como en el teorema 3.3
betas <- matrix(model_stratio_classeng$coefficients)

# Quiero que los betas 3, 4 y 5 sean iguales a 0
A <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
              0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0,1), nrow = 4)
A
C <- matrix(c(0, 0, 0, 0), nrow = 4)
C

# La matriz del modelo
X <- model.matrix(model_stratio_classeng)
X

# Calculo el F
F_val <- t(A %*% betas) %*% solve(A %*% solve(t(X) %*% X) %*% t(A)) %*% (A %*% betas) / (nrow(A)*summary(model_stratio_classeng)$sigma^2)
F_val

# Y ahora el p-value
pf(df1 = nrow(A), df2 = nrow(X)-ncol(X), q = as.numeric(F_val), lower.tail = FALSE)

# La interacción no es estadísticamente significativa

# El modelo es:
# score = 678.5267 - 0.6842 * stratio # Para classeng == 1
# score = 664.3459 - 0.3129 * stratio # Para classeng == 2
# score = 704.9644 - 2.6783 * stratio # Para classeng == 3
# score = 675.4086 - 1.7197 * stratio # Para classeng == 4
# score = 615.7753 + 0.5202 * stratio # Para classeng == 5

data_rectas <- tibble(
  classeng = 1:5,
  ordenadas = c(model_stratio_classeng$coefficients[1], 
                model_stratio_classeng$coefficients[1] +  model_stratio_classeng$coefficients[3:6]),
  pendientes = c(model_stratio_classeng$coefficients[2], 
                 model_stratio_classeng$coefficients[2] +  model_stratio_classeng$coefficients[7:10])) %>%
  mutate(classeng = factor(classeng))
data_rectas

CASchools %>% ggplot(aes(x = stratio,
                         y = score,
                         color = classeng)) +
  geom_point(alpha = .5) +
  geom_abline(data = data_rectas,
              aes(intercept = ordenadas,
                  slope = pendientes,
                  color = classeng)) +
  scale_color_manual(values = colormap) +
  theme_bw() +
  theme(legend.position = "top")

## e ####
model_stratio_english <- lm(data = CASchools, 
                            score ~ stratio + english)
summary(model_stratio_english)
# El modelo ajustado es:
# score = 686.03224 - 1.10130 * stratio - 0.64978 * english

# Ambos coeficientes son significativos y coherenetes. A menos docentes por estudiantes
# menor score y a mas alumnos con inglés no nativos menor score.

## f ####
model_stratio_lunch <- lm(data = CASchools, 
                          score ~ stratio + lunch)
summary(model_stratio_lunch)
# El modelo ajustado es:
# score = 702.91130 - 1.11723 * stratio - 0.59975 * lunch

# Ahora son significativas tanto la pendiente con stratio como el cambio debido a lunch.
# Ambos resultados tiene sentido, familias deventajadas tienen scores más bajos y al 
# aumentar el stratio disminuye score.

## g ####
CASchools <- CASchools %>%
  mutate(compst = computer/students)

model_stratio_english_comp <- lm(data = CASchools, 
                                 score ~ stratio + english + compst)

## h ####
summary(model_stratio_english_comp)
# El modelo ajustado es:
# score = 677.0642 - 0.8490 * stratio - 0.6304 * english + 27.2696 * compst

# Todas las variables son significativas
sigma(model_stratio_english_comp)

new_data <- tibble(stratio = 20., compst = 0.10, english = 15.)
predict(model_stratio_english_comp, newdata = new_data)
predict(model_stratio_english_comp, newdata = new_data, interval = "confidence")
predict(model_stratio_english_comp, newdata = new_data, interval = "prediction")

# A mano
betas <- matrix(model_stratio_english_comp$coefficients)
xh <- matrix(c(1, 20, 15, .1))
t(xh) %*%  betas # Valor predicho

X <- model.matrix(model_stratio_english_comp)

n <- nrow(CASchools)
p <- ncol(model.matrix(model_stratio_english_comp))

# Intervalos de confianza
alpha <- .05
t(xh) %*%  betas + qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model_stratio_english_comp)  * sqrt(t(xh) %*% solve(t(X)%*%X) %*% xh)
t(xh) %*%  betas - qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model_stratio_english_comp)  * sqrt(t(xh) %*% solve(t(X)%*%X) %*% xh)

# Intervalos de prediccion
alpha <- .05
t(xh) %*%  betas +  qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model_stratio_english_comp)  * sqrt(1 + t(xh) %*% solve(t(X)%*%X) %*% xh)
t(xh) %*%  betas -  qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model_stratio_english_comp)  * sqrt(1 + t(xh) %*% solve(t(X)%*%X) %*% xh)

## i ####
model_stratio_full <- lm(data = CASchools, 
                         score ~ stratio * english * compst)
summary(model_stratio_full)
car::Anova(model_stratio_full, type = 3)
