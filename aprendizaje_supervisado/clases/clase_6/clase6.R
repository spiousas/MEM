pacman::p_load(ISLR2, tidyverse, gam, patchwork, rsample, splines)

head(Wage)
summary(Wage)

# Armo una x de edades
edades <- 18:80

#Ajustamos una regresión polinómica de grado 4
fit <- lm( wage ~ poly(age,4), data = Wage ) # Ajusta con una base ortogonal de polinomios
fit2 <- lm( wage ~ I(age) + I(age^2) + I(age^3) + I(age^4), data = Wage ) # Ajusta con los polinomios a lo bestia
# Las predicciones son iguales!

fit3 <- lm( wage ~ poly(age, 4, raw = T), data = Wage ) # Con raw=TRUE da igual que fit2

#Ajustamos una regresión logista para predecir ingresos mayores a 250
glmfit <- glm( I(wage > 250) ~ poly(age,4), data = Wage, family = binomial)
glmfit

pred_poly <- predict(fit, newdata = tibble(age = edades), se = T) # se = TRUE da desvio estandar en cada punto.

pred_poly_tbl <- tibble(age = edades, 
                        pred_wage = pred_poly$fit, 
                        pred_se = pred_poly$se.fit)

p_poly <- Wage %>% 
  ggplot(aes(x = age,
             y = wage)) +
  geom_point(alpha = .5) +
  geom_line(data = pred_poly_tbl, aes(y = pred_wage),
            linewidth = 2, color = "brown") +
  geom_ribbon(data = pred_bs_tbl, aes(y = pred_wage, ymin = pred_wage - pred_se, ymax = pred_wage + pred_se),
              alpha = .5, fill = "brown")+
  labs(title = "Predicción con poly",
       x = "Edad",
       y = "Salario") +
  theme_bw()
p_poly

#Ajustamos por funciones escalon
fit_escalon = lm( wage ~ I( 30<= age & age < 50) + I( 50<= age & age < 70) + I( 70<= age), data =Wage )
fit_escalon2 = lm( wage ~ I(cut(age, 4)), data =Wage )

summary(fit_escalon2)
summary(fit_escalon)

pred_escalon1 <- predict(fit_escalon, newdata = tibble(age = edades), se = T) # se = TRUE da desvio estandar en cada punto.
pred_escalon2 <- predict(fit_escalon2, newdata = tibble(age = edades), se = T) # se = TRUE da desvio estandar en cada punto.

pred_escalon_tbl <- tibble(age = edades, 
                           pred_escalon1 = pred_escalon1$fit,
                           pred_escalon2 = pred_escalon2$fit)

p_escalon <- Wage %>% 
  ggplot(aes(x = age,
             y = wage)) +
  geom_point(alpha = .5) +
  geom_line(data = pred_escalon_tbl, aes(y = pred_escalon1),
            linewidth = 2, color = "brown") +
  geom_line(data = pred_escalon_tbl, aes(y = pred_escalon2),
            linewidth = 2, color = "dodgerblue4") +
  labs(title = "Predicción con Escalón",
       x = "Edad",
       y = "Salario") +
  theme_bw()
p_escalon


# Ajustamos con splines
fit_spline <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage) # bs es la base de splines
pred_bs <- predict(fit, newdata = tibble(age = edades), se = T) # se = TRUE da desvio estandar en cada punto.

pred_bs_tbl <- tibble(age = edades, 
                   pred_wage = pred_bs$fit, 
                   pred_se = pred_bs$se.fit)

p_spline <- p_poly <- Wage %>% 
  ggplot(aes(x = age,
             y = wage)) +
  geom_point(alpha = .5) +
  geom_line(data = pred_bs_tbl, aes(y = pred_wage),
            linewidth = 2, color = "dodgerblue4") +
  geom_ribbon(data = pred_bs_tbl, aes(y = pred_wage, ymin = pred_wage - pred_se, ymax = pred_wage + pred_se),
              alpha = .5, fill = "dodgerblue4")+
  labs(title = "Predicción con splines",
       x = "Edad",
       y = "Salario") +
  theme_bw()
p_spline
  
# Ajustamos con smoothing splines. Podemos elegir por df o pedir validacion cruzada loocv
fit_natural_spline <- lm(wage ~ ns(age, df = 6), data = Wage) # ns es natural spline
pred_ns <- predict(fit, newdata = list(age = edades), se = T) 

pred_ns_tbl <- tibble(age = edades, 
                      pred_wage = pred_ns$fit, 
                      pred_se = pred_ns$se.fit)

p_nspline <- p_spline +
  geom_line(data = pred_ns_tbl, aes(y = pred_wage),
            linewidth = 2, color = "darkolivegreen") +
  geom_ribbon(data = pred_ns_tbl, aes(y = pred_wage, ymin = pred_wage - pred_se, ymax = pred_wage + pred_se),
              alpha = .5, fill = "darkolivegreen") 
p_nspline

# Ajuste gam
gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)
summary(gam1)
gam.m3 <- gam(wage ~ s(year, 4) + ns(age, 5) + education,
              data = Wage)
summary(gam.m3)

# Logistic regression with gam
gam.lr <- gam( I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage)

par(mfrow = c(1, 3))
plot(gam.lr, se = T, col = "green")

plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey") > title("Smoothing Spline")
fit_smooth <- with( Wage, smooth.spline(age, wage, df = 16))
fit_smooth2 <-  with( Wage, smooth.spline(age, wage, cv = TRUE))
fit_smooth2$cv.crit

plot(fit_smooth2, type = 'l')

## Graficos del ISLR
with(Wage, plot(age, wage, col = 'gray'))
lines(edades, predict(fit, newdata = data.frame( age = edades) ), type = 'l', col = 'blue', lwd =2 ) 
lines(edades, predict(fit_escalon, newdata = data.frame( age = edades) ), type = 'l', col = 'green', lwd =3 ) 
lines(18:80, predict(fit_escalon2, newdata = data.frame( age = 18:80) ), type = 'l', col = 'magenta', lwd =3 ) 

lines(edades, pred$fit, lwd = 2)
lines(edades, pred$fit + 2 * pred$se, lty = "dashed")
lines(edades, pred$fit - 2 * pred$se, lty = "dashed")

lines(edades, pred_ns$fit, lwd = 2, col= 'orange')
lines(edades, pred_ns$fit + 2 * pred$se, lty = "dashed", col ='orange')
lines(edades, pred_ns$fit - 2 * pred$se, lty = "dashed", col ='orange')

with(Wage, plot(age, wage >= 250))

lines(edades, predict(glmfit, newdata = data.frame(age = edades), type = 'response' ),col = 'blue', lwd=2)