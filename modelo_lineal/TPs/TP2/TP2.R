pacman::p_load(here, tidyverse, openintro, corrplot, alr4)

# Ejercicio 4 ####
data_vapor <- read_csv(here("modelo_lineal/TPs/TP2/data/vapor.TXT"))
data_vapor

## a ####
data_vapor %>% ggplot(aes(x = x1,
                          y = y)) +
  geom_point() +
  theme_bw()

# Se observa una relación lineal decreciente

## b ####
data_vapor %>%
  summarise(m_x1 = mean(x1),
            m_y = mean(y),
            sd_x1 = sd(x1),
            sd_y = sd(y),)

## c ####
beta1_hat <- cov(data_vapor$x1, data_vapor$y)/var(data_vapor$x1)
beta0_hat <- mean(data_vapor$y) - beta1_hat * mean(data_vapor$x1)
beta1_hat
beta0_hat

modelo <- lm(data = data_vapor, y ~ x1)

## d ####
n <- nrow(data_vapor)
res <- data_vapor$y - (beta0_hat + beta1_hat * data_vapor$x1)
sqrt(sum(res^2)/(n-2))
sigma(modelo)

## e ####
vcov(modelo)

# Ahora a mano
sigma2 <- sum(res^2)/(n-2)
var_beta_1 <- sigma2/((n-1)*var(data_vapor$x1))
var_beta_1

var_beta_0 <- sigma2 * (1/n + mean(data_vapor$x1)^2/((n-1)*var(data_vapor$x1)))
var_beta_0

cov_beta_0_beta_1 <- -sigma2 * mean(data_vapor$x1)/((n-1)*var(data_vapor$x1))
cov_beta_0_beta_1

## f ####
sum(data_vapor$y-modelo$fitted.values)

## g ####
data_vapor <- data_vapor %>%
  mutate(x_center = x1 - mean(x1))

modelo_center <- lm(data = data_vapor, y~x_center)
summary(modelo_center)

modelo$coefficients
modelo_center$coefficients
# Puede observarse que, como es de esperarse, las pendientes no cambian.

# El beta_0_hat deberia ser el anterior + beta_1_hat*mean(x1)
modelo$coefficients[1] + modelo_center$coefficients[2] * mean(data_vapor$x1)
modelo_center$coefficients[1]

# El estimador de sigma no deberia cambiar
sigma(modelo)
sigma(modelo_center)

# La matriz de covarianza debería cambiar, disminuyendo la varianza(beta0) y 
# llevando a cero la cruzada (esto porque aparece mean(x_center) en sus calculos)
vcov(modelo)
vcov(modelo_center)

var_beta_1 <- sigma2/((n-1)*var(data_vapor$x_center))
var_beta_1

var_beta_0 <- sigma2 * (1/n + mean(data_vapor$x_center)^2/((n-1)*var(data_vapor$x_center)))
var_beta_0

cov_beta_0_beta_1 <- -sigma2 * mean(data_vapor$x_center)/((n-1)*var(data_vapor$x_center))
cov_beta_0_beta_1

# Ejercicio 5 ####
data(bdims, package = "openintro")

## a ####
bdims %>% ggplot(aes(x = hip_gi,
                     y = wgt)) +
  geom_point() +
  theme_bw()
  
## b ####
bdims %>% ggplot(aes(x = hip_gi * 2.20462,
                     y = wgt)) +
  geom_point() +
  theme_bw()

## c ####
modelo_peso <- lm(data = bdims, wgt ~ hip_gi)
summary(modelo_peso)

## d ####
bdims %>% ggplot(aes(x = hip_gi,
                     y = wgt)) +
  geom_point() +
  geom_abline(intercept = modelo_peso$coefficients[1],
              slope = modelo_peso$coefficients[2],
              color = "red", linewidth = 1) +
  theme_bw()
# La recta describe bien la pendiente, pero parece haber una diferencia de 
# ordenada al origen de dos grupo (me la juego que es sexo)

## e ####
new_data <- tibble(hip_gi = 100)
predict(modelo_peso, newdata = new_data)

## f ####
81 - predict(modelo_peso, newdata = new_data)

## g ####
predict(modelo_peso, newdata = new_data)

## h ####
sum(modelo_peso$residuals^2)/(nrow(bdims)-2)
sigma(modelo_peso)^2

## i ####
confint(modelo_peso)

# A mano y usando el pivot
n <- nrow(bdims)
p <- 2
betas <- modelo_peso$coefficients
alpha <- .05
summary(modelo_peso)$coefficients[2,1] + qt(p = alpha/2, df = n-p, lower.tail = F) * summary(modelo_peso)$coefficients[2,2]
summary(modelo_peso)$coefficients[2,1] - qt(p = alpha/2, df = n-p, lower.tail = F) * summary(modelo_peso)$coefficients[2,2]

# Ejercicio 6 ####
data("Heights")
help("Heights")

## a.i ####
Heights %>% ggplot(aes(x = mheight,
           y = dheight)) +
  geom_point(alpha = .5) +
  theme_bw()

## a.ii ####  
# El scatterplot sería una recta de pendiente 1
Heights %>% ggplot(aes(x = mheight,
                       y = mheight)) +
  geom_point(alpha = .5) +
  theme_bw()
# Esta gráfica habla sólamente de las Xs y no dice nada de la relación entre las 
# alturas de madres e hijas.


## a.ii ####  
# El scatterplot sería una recta de pendiente 1
Heights %>% ggplot(aes(x = mheight,
                       y = dheight + runif(n = nrow(Heights), min = -.5, max = .5))) +
  geom_point(alpha = .5) +
  theme_bw()
# Esta gráfica habla sólamente de las Xs y no dice nada de la relación entre las 
# alturas de madres e hijas.

## b ####
modelo <- lm(data = Heights, dheight ~ mheight)
summary(modelo)

Heights %>% ggplot(aes(x = mheight,
                       y = dheight)) +
  geom_point(alpha = .5) +
  geom_abline(intercept = 0, slope = 1, color = "blue", linewidth = 1) +
  geom_abline(intercept = modelo$coefficients[1], slope = modelo$coefficients[2], color = "red", linewidth = 1) +
  theme_bw()
