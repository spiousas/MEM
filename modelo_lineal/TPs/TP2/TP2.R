pacman::p_load(here, tidyverse, openintro, corrplot, alr4)

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
