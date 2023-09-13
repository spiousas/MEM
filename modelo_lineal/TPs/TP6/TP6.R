pacman::p_load(tidyverse, here, AER, GGally, ellipse, janitor)
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
sigma <- sigma(model_grado1)
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
q <- nrow(C)
pf(df1 = q, df2 = n-p, q = as.numeric(F_val), lower.tail = FALSE)

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
summary(model_stratio)

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

# Ejercicio 3 ####
data_growth <- read_delim(here("modelo_lineal/TPs/TP6/data/growth.txt"))

## a ####
data_growth %>% ggplot(aes(x = supplement,
                           y = gain,
                           fill = diet)) +
  stat_summary(geom = "bar", position = position_dodge2()) +
  scale_fill_manual(values = colormap) +
  theme_bw() +
  theme(legend.position = "top")
  
## b ####
model_growth <- lm(data = data_growth,
                   gain ~ diet * supplement)
summary(model_growth)

## c ####
interaction.plot(data_growth$diet,data_growth$supplement,data_growth$gain)
interaction.plot(data_growth$supplement,data_growth$diet,data_growth$gain)

data_growth %>% ggplot(aes(x = diet,
                           y = gain,
                           color = supplement)) +
  stat_summary() +
  scale_color_manual(values = colormap) +
  theme_bw() +
  theme(legend.position = "top")

## d ####
model_growth_no_int <- lm(data = data_growth,
                          gain ~ diet + supplement)
summary(model_growth_no_int)

anova(model_growth_no_int, model_growth)

# Plantenado todo como en el teorema 3.3
betas <- matrix(model_growth$coefficients)

# Quiero que los betas 3, 4 y 5 sean iguales a 0
A <- matrix(c(0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0,
              1, 0, 0, 0, 0, 0,
              0, 1, 0, 0, 0, 0,
              0, 0, 1, 0, 0, 0,
              0, 0, 0, 1, 0, 0,
              0, 0, 0, 0, 1, 0,
              0, 0, 0, 0, 0, 1), nrow = 6)
A
C <- matrix(c(0, 0, 0, 0, 0, 0), nrow = 6)
C

# La matriz del modelo
X <- model.matrix(model_growth)
X

# Calculo el F
F_val <- t(A %*% betas) %*% solve(A %*% solve(t(X) %*% X) %*% t(A)) %*% (A %*% betas) / (nrow(A)*summary(model_growth)$sigma^2)
F_val

# Y ahora el p-value
pf(df1 = nrow(A), df2 = nrow(X)-ncol(X), q = as.numeric(F_val), lower.tail = FALSE)

## e ####
model_growth_no_int <- lm(data = data_growth,
                          gain ~ diet + supplement)
summary(model_growth_no_int)

## f ####
model_growth_no_supplement <- lm(data = data_growth,
                                 gain ~ diet)
summary(model_growth_no_supplement)

anova(model_growth_no_int, model_growth_no_supplement)

# Plantenado todo como en el teorema 3.3
betas <- matrix(model_growth_no_int$coefficients)

# Quiero que los betas 3, 4 y 5 sean iguales a 0
A <- matrix(c(0, 0, 0,
              0, 0, 0,
              0, 0, 0,
              1, 0, 0,
              0, 1, 0,
              0, 0, 1), nrow = 3)
A
C <- matrix(c(0, 0, 0), nrow = 3)
C

# La matriz del modelo
X <- model.matrix(model_growth_no_int)
X

# Calculo el F
F_val <- t(A %*% betas) %*% solve(A %*% solve(t(X) %*% X) %*% t(A)) %*% (A %*% betas) / (nrow(A)*summary(model_growth_no_int)$sigma^2)
F_val

# Y ahora el p-value
pf(df1 = nrow(A), df2 = nrow(X)-ncol(X), q = as.numeric(F_val), lower.tail = FALSE)

## g ####
new_data <- tibble(diet = c("oats", "barley"),
                   supplement = c("supersupp", "agrimore"))
new_data

predict(model_growth_no_int, newdata = new_data)

# Ejercicio 4 ####
data(stackloss)
stackloss <- tibble(stackloss) %>%
  clean_names()
stackloss

## a ####
# R base
pairs(stackloss, font.labels = 3, font.axis = 5, pch = 19)
# ggplot2
ggpairs(stackloss) +
  theme_bw()

## b ####
model_stackloss <- lm(data = stackloss,
                      stack_loss ~ air_flow + water_temp + acid_conc)
summary(model_stackloss)

## c ####
summary(model_stackloss)
# Los parámetros dan todos significativos salvo acid_conc
# DUDA: A qué se refiere con "Qué modelo se considera en cada test"?
# Lo que podría estar pasando es que hay colinealidad entre acid_conc y el resto de las variables

## d ####
model_stackloss_simple <- lm(data = stackloss, 
                             stack_loss ~ air_flow + water_temp)
summary(model_stackloss_simple)
# Si miramos el R ajustado es un modelo "mejor"

## e ####
confint(model_stackloss, level = .9)

# A mano
betas <- matrix(model_stackloss$coefficients)
X <- model.matrix(model_stackloss)

n <- nrow(model.matrix(model_stackloss))
p <- ncol(model.matrix(model_stackloss))

alpha <- .1

# Intervalos de confianza para el beta_1
a <- matrix(c(1, 0, 0, 0))
t(a) %*%  betas # Valor predicho

t(a) %*%  betas + qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(t(a) %*% Matrix::solve(t(X)%*%X) %*% a)
t(a) %*%  betas - qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)

# Intervalos de confianza para el beta_2
a <- matrix(c(0, 1, 0, 0))
t(a) %*%  betas # Valor predicho

t(a) %*%  betas + qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(t(a) %*% Matrix::solve(t(X)%*%X) %*% a)
t(a) %*%  betas - qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)

# Intervalos de confianza para el beta_3
a <- matrix(c(0, 0, 1, 0))
t(a) %*%  betas # Valor predicho

t(a) %*%  betas + qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(t(a) %*% Matrix::solve(t(X)%*%X) %*% a)
t(a) %*%  betas - qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)

# Intervalos de confianza para el beta_4
a <- matrix(c(0, 0, 0, 1))
t(a) %*%  betas # Valor predicho

t(a) %*%  betas + qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(t(a) %*% Matrix::solve(t(X)%*%X) %*% a)
t(a) %*%  betas - qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)

## f ####
new_data <- tibble(air_flow = 58, water_temp = 20, acid_conc = 86)
new_data

predict(model_stackloss, newdata = new_data, interval = "confidence", level = .95)

# A mano
# Intervalos de confianza para air_flow = 58, water_temp = 20, acid_conc = 86
a <- matrix(c(1, 58, 20, 86))
t(a) %*%  betas # Valor predicho

alpha <- .05
t(a) %*%  betas + qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)
t(a) %*%  betas - qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)

## g ####
predict(model_stackloss, newdata = new_data, interval = "prediction", level = .99)

# A mano
# Intervalos de confianza para air_flow = 58, water_temp = 20, acid_conc = 86
a <- matrix(c(1, 58, 20, 86))
t(a) %*%  betas # Valor predicho

alpha <- .01
t(a) %*%  betas + qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(1 + t(a) %*% solve(t(X)%*%X) %*% a)
t(a) %*%  betas - qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(1 + t(a) %*% solve(t(X)%*%X) %*% a)

## h ####

# Quiero que beta2 sea distinto a 0
a <- matrix(c(0, 0, 1, 0), nrow = 4)
a
c <- 2
c

# La matriz del modelo
X <- model.matrix(model_stackloss)
X

# Calculo el F
TE_val <- (t(a) %*% betas - c) / sqrt(sigma(model_stackloss)^2 *t(a) %*% solve(t(X)%*%X) %*% a)
TE_val

# Y ahora el p-value
pvalor <- pt(TE_val, df=n-p, lower.tail = F)
pvalor

## i ####
# Usando la librería ellipse
data_ellipse <- as.tibble(ellipse(model_stackloss, which=c(2,3), level=.90),)
plot_ellipse <- data_ellipse %>% 
  ggplot(aes(x = air_flow,
           y = water_temp)) +
  geom_path() +
  geom_point(x = coef(model_stackloss)[2], y = coef(model_stackloss)[3], size =2,color="red") +
  theme_bw()
plot_ellipse

# Bonferroni
q <- 2 # Cantidad de combinaciones lineales (0,0,1,0) y (0,1,0,0)
alpha <- 0.1/(2*q)
CIs_bonf <- confint(model_stackloss, level = 1-alpha)

plot_bonferroni <- plot_ellipse +
  geom_vline(xintercept = CIs_bonf[2,], color = colormap[1]) +
  geom_hline(yintercept = CIs_bonf[3,], color = colormap[1])
plot_bonferroni

# A mano 
betas <- matrix(model_stackloss$coefficients)
X <- model.matrix(model_stackloss)

n <- nrow(model.matrix(model_stackloss))
p <- ncol(model.matrix(model_stackloss))

# Intervalos de confianza para el beta_2
a <- matrix(c(0, 1, 0, 0))
t(a) %*%  betas # Valor predicho

t(a) %*%  betas + qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)
t(a) %*%  betas - qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)

# Intervalos de confianza para el beta_3
a <- matrix(c(0, 0, 1, 0))
t(a) %*%  betas # Valor predicho

t(a) %*%  betas + qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)
t(a) %*%  betas - qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)

# Scheffe a mano 
alpha <- 0.1
CIs_scheffe <- CIs_bonf[2:3,]

# Intervalos de confianza para el beta_2
a <- matrix(c(0, 1, 0, 0))
t(a) %*%  betas # Valor predicho

CIs_scheffe[1,1] <- t(a) %*%  betas - sqrt(p*qf(p = alpha, df1 = p, df2 = n-p, lower.tail = F)) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)
CIs_scheffe[1,2] <- t(a) %*%  betas + sqrt(p*qf(p = alpha, df1 = p, df2 = n-p, lower.tail = F)) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)

# Intervalos de confianza para el beta_2
a <- matrix(c(0, 0, 1, 0))
t(a) %*%  betas # Valor predicho

CIs_scheffe[2,1] <- t(a) %*%  betas - sqrt(p*qf(p = alpha, df1 = p, df2 = n-p, lower.tail = F)) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)
CIs_scheffe[2,2] <- t(a) %*%  betas + sqrt(p*qf(p = alpha, df1 = p, df2 = n-p, lower.tail = F)) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)

plot_scheffe <- plot_bonferroni +
  geom_vline(xintercept = CIs_scheffe[1,], color = colormap[2]) +
  geom_hline(yintercept = CIs_scheffe[2,], color = colormap[2])
plot_scheffe

# Sin corregir a mano 
alpha <- 0.1
CIs_elipse <- CIs_scheffe

# Intervalos de confianza para el beta_2
a <- matrix(c(0, 1, 0, 0))
t(a) %*%  betas # Valor predicho

CIs_elipse[1,1] <- t(a) %*%  betas - qf(p = alpha, df1 = p, df2 = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)
CIs_elipse[1,2] <- t(a) %*%  betas + qf(p = alpha, df1 = p, df2 = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)

# Intervalos de confianza para el beta_2
a <- matrix(c(0, 0, 1, 0))
t(a) %*%  betas # Valor predicho

CIs_elipse[2,1] <- t(a) %*%  betas - qf(p = alpha, df1 = p, df2 = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)
CIs_elipse[2,2] <- t(a) %*%  betas + qf(p = alpha, df1 = p, df2 = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)

plot_todos <- plot_scheffe +
  geom_vline(xintercept = CIs_elipse[1,], color = colormap[4]) +
  geom_hline(yintercept = CIs_elipse[2,], color = colormap[4])
plot_todos

## j ####
# Usando la librería ellipse
data_ellipse <- as.tibble(ellipse(model_stackloss, which=c(3,4), level=.90),)
plot_ellipse <- data_ellipse %>% 
  ggplot(aes(x = water_temp,
             y = acid_conc)) +
  geom_path() +
  geom_point(x = coef(model_stackloss)[3], y = coef(model_stackloss)[4], size =2,color="red") +
  theme_bw()
plot_ellipse

# Bonferroni
q <- 2 # Cantidad de combinaciones lineales (0,0,1,0) y (0,1,0,0)
alpha <- 0.1/q
CIs_bonf <- confint(model_stackloss, level = 1-alpha)
CIs_bonf

plot_bonferroni <- plot_ellipse +
  geom_vline(xintercept = CIs_bonf[3,], color = colormap[1]) +
  geom_hline(yintercept = CIs_bonf[4,], color = colormap[1])
plot_bonferroni

# Scheffe a mano 
alpha <- 0.1
CIs_scheffe <- CIs_bonf[3:4,]

# Intervalos de confianza para el beta_2
a <- matrix(c(0, 0, 1, 0))
t(a) %*%  betas # Valor predicho

CIs_scheffe[1,1] <- t(a) %*%  betas - sqrt(p * qf(p = alpha, df1 = p, df2 = n-p, lower.tail = F)) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)
CIs_scheffe[1,2] <- t(a) %*%  betas + sqrt(p * qf(p = alpha, df1 = p, df2 = n-p, lower.tail = F)) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)

# Intervalos de confianza para el beta_2
a <- matrix(c(0, 0, 0, 1))
t(a) %*%  betas # Valor predicho

CIs_scheffe[2,1] <- t(a) %*%  betas - sqrt(p * qf(p = alpha, df1 = p, df2 = n-p, lower.tail = F)) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)
CIs_scheffe[2,2] <- t(a) %*%  betas + sqrt(p * qf(p = alpha, df1 = p, df2 = n-p, lower.tail = F)) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)

plot_scheffe <- plot_bonferroni +
  geom_vline(xintercept = CIs_scheffe[1,], color = colormap[2]) +
  geom_hline(yintercept = CIs_scheffe[2,], color = colormap[2])
plot_scheffe

# Sin corregir a mano 
alpha <- 0.1
CIs_elipse <- CIs_scheffe

# Intervalos de confianza para el beta_2
a <- matrix(c(0, 0, 1, 0))
t(a) %*%  betas # Valor predicho

CIs_elipse[1,1] <- t(a) %*%  betas - qf(p = alpha, df1 = p, df2 = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)
CIs_elipse[1,2] <- t(a) %*%  betas + qf(p = alpha, df1 = p, df2 = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)

# Intervalos de confianza para el beta_2
a <- matrix(c(0, 0, 0, 1))
t(a) %*%  betas # Valor predicho

CIs_elipse[2,1] <- t(a) %*%  betas - qf(p = alpha, df1 = p, df2 = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)
CIs_elipse[2,2] <- t(a) %*%  betas + qf(p = alpha, df1 = p, df2 = n-p, lower.tail = F) *  sigma(model_stackloss)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)

plot_todos <- plot_scheffe +
  geom_vline(xintercept = CIs_elipse[1,], color = colormap[4]) +
  geom_hline(yintercept = CIs_elipse[2,], color = colormap[4])
plot_todos

