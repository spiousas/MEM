pacman::p_load(ISLR2, tidyverse, patchwork)

# Los datos de Bikeshare ####
head(Bikeshare)

## No hay observaciones para todos los dias y todas las horas... pero casi
Bikeshare %>% 
  summarize(n_distinct(mnth, day, hr)) / (365*24)

## Faltan de manera aleatoria?
Bikeshare %>% 
  count(day) %>% 
  ggplot(aes(x = n)) +
  geom_bar(fill = "steelblue") +
  ggtitle('Graficos de barra por cantidad de dias con n observaciones') +
  theme_bw()

# Transformo los datos agrupando por mes
data_transformada = Bikeshare %>% 
  group_by(mnth) %>% 
  summarise( avg_bikers = mean( bikers),
             hour_sup_sd = mean( bikers) + sd(bikers),
             hour_inf_sd = mean( bikers) - sd(bikers)) 


p1 = ggplot(data_transformada, aes(x = mnth, y = avg_bikers, group = 1 )) + 
  geom_ribbon(aes(x = mnth, ymin = hour_inf_sd, ymax = hour_sup_sd), alpha =.5, fill = "steelblue") + 
  geom_line(linewidth = 1.5, alpha = .7) + 
  geom_point(size = 1.5) + 
  ggtitle('Promedio de bikers +- 1 desvio') +
  theme_bw()

# Transformo los datos agrupando por hora
data_transformada_2 = Bikeshare %>% 
  group_by(hr) %>% 
  summarise( avg_bikers = mean( bikers),
             hour_sup_sd = mean( bikers) + sd(bikers),
             hour_inf_sd = mean( bikers) - sd(bikers),
  ) 

p2 = ggplot(data_transformada_2, aes(x = hr, y = avg_bikers, group = 1 )) + 
  geom_ribbon(aes(x = hr, ymin = hour_inf_sd, ymax = hour_sup_sd), alpha =.5, fill = "steelblue") + 
  geom_line(linewidth = 1.5, alpha = .7) + 
  geom_point(size = 1.5) + 
  ggtitle('Promedio de bikers +- 1 desvio') +
  theme_bw()   

p1 + p2

#Graficos alternativos
ggplot(Bikeshare, aes(y = bikers, x = mnth)) + 
  geom_boxplot(fill = "steelblue", alpha = .5) + 
  ggtitle('Boxplot de bikers por mes') +  
  theme_bw() +
ggplot(Bikeshare, aes(y = bikers, x = hr)) + 
  geom_boxplot(fill = "steelblue", alpha = .5) + 
  ggtitle('Boxplot de bikers por hora') +
  theme_bw()

ggplot(Bikeshare %>% filter(hr == 17), aes(y = bikers, x = as.factor(workingday) )) + geom_boxplot() 

# Solo para mostrar por que ggplot es tan lindo
data_transformada_3 = Bikeshare %>% 
  mutate(season = as.factor(season), 
         workingday = ifelse(workingday, 'Dias laborables', 'Dias no laborables')) %>% 
   group_by(hr, mnth, workingday) %>% 
   summarise( avg_bikers = mean( bikers),
              sd_bikers = sd(bikers),
   ) 

p4 = ggplot(data_transformada_3, aes(x = hr, y = avg_bikers, group = mnth, color = mnth )) + 
  geom_line() + 
  labs(x = "Hora", 
       y = "# bikers", 
       title = 'Promedio de bikers por hora en distintos meses',
       color = "Mes") + 
  facet_wrap( ~ workingday) +
  theme_bw()
p4

p5 = ggplot(Bikeshare, aes(x = weathersit, y = bikers )) + 
  geom_boxplot(fill = "steelblue", alpha = .5) +
  ggtitle('Bikers por clima') +
  theme_bw()
p5

# Ajustar una regresion de poisson en el proble de los ciclistas ####

mod.pois <- glm( bikers ~ mnth + hr + workingday + temp + weathersit, data = Bikeshare, family = poisson)
mod.lm = lm( bikers ~ mnth + hr + workingday + temp + weathersit, data = Bikeshare)

y_hat <- predict( mod.pois , newdata =  Bikeshare %>% select(mnth, hr, workingday, temp, weathersit))
y <- mod.pois$fitted.values
head(log(y))
head(y_hat)

par(mfrow = c(1,1))
plot(mod.lm)
summary(mod.pois)

coef.mnth = c(0, coef(mod.pois)[2:12])

par(mfrow = c(1,2))
plot(1:12, coef.mnth, xlab = 'Mes', ylab = 'Coeficiente', xaxt = 'n', col = 'blue', pch = 19, type = 'o', main = 'Coeficiente estimado por mes')
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))

coef.hours <- c(0, coef(mod.pois)[13:35])
plot(coef.hours, xlab = "Hour", ylab = "Coefficient", col = "blue", pch = 19, type = "o", main ='Coeficientes estimado por hora del dia')

par(mfrow = c(1,1))

with(mod.pois, plot(fitted.values, Bikeshare$bikers - fitted.values, main = 'Fitted vs residuals' ) ) 