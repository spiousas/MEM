pacman::p_load(tidyverse, here)

titanic <- read_tsv(here("aprendizaje_estadistico/Pr_1/data/titanic.csv")) %>%
  mutate(age = as.integer(age),
         sibsp = as.integer(sibsp),
         parch = as.integer(parch))
head(titanic)
tail(titanic)

ncol(titanic)
nrow(titanic)

str(titanic)

count(titanic, pclass)

titanic %>% 
  group_by(pclass) %>%
  summarise(prop_survived = sum(survived==1)/n())

titanic %>% 
  group_by(sex) %>%
  summarise(prop_survived = sum(survived==1)/n())
  
titanic %>% 
  summarise(median_fare = median(fare, na.rm = T),
            mean_fare = mean(fare, na.rm = T))

titanic %>% 
  group_by(pclass) %>%
  summarise(median_fare = median(fare, na.rm = T),
            mean_fare = mean(fare, na.rm = T))

titanic %>% 
  drop_na() %>%
  ggplot(aes(x = factor(pclass),
             y = fare)) +
  geom_point(position = position_jitter(width = .2), alpha =.2) +
  labs(x = "Cabina",
       y = "Tarifa",
       title = "Relación entre tipo de cabina y edad",
       subtitle = str_wrap("La relación entre tarifa y clase es bastante extraña porque si 
                           bien las medias son 8.66, 15.6 y 61.4, aumentando a medida que 
                           aumentamos el nivel de la clase (de 3 a 1), las medianas son muy 
                           raras 2434, 203 y 12424 para las mismas clases. Esto muestra 
                           que hay muchas tarifas anómalas en cada clase (outliers). 
                           Para este tipo de distribución de los datos, con tanta masa alrededor 
                           de unos valores y con outliers que están a órdenes de magnitud de 
                           distancia no tiene sentido graficar un boxplot (por eso no lo incluí en la figura) 
                           ya que considera a la mayoría de los puntos que vemos separados como outliers. 
                           Tal vez una transformación log nos ayudaría a visualizarlos mejor.", width = 100),
       caption = "Ignacio Spiousas") +
  scale_y_continuous(labels=scales::dollar_format()) +
  theme_bw() +
  theme(plot.subtitle=element_text(size=10),
        plot.title.position = "plot")

ggsave(here("aprendizaje_estadistico/Pr_1/figs/cabina_tarifa.pdf"), width = 6.6, height = 6, units = "in")
    
titanic %>% 
  group_by(pclass) %>%
  summarise(median_fare = median(age, na.rm = T),
            mean_fare = mean(age, na.rm = T))

titanic %>% 
  drop_na() %>%
  ggplot(aes(x = factor(pclass),
             y = age)) +
  geom_boxplot() +
  geom_point(position = position_jitter(width = .2), alpha = .1) +
  labs(x = "Cabina",
       y = "Edad",
       title = "Relación entre tipo de cabina y edad",
       subtitle = str_wrap("Por otro lado, la relación entre edad y cabina es relativamente razonable. 
                           La 3era clase tiene una edad promedio de 24.5 años (mediana = 24), 
                           la 2nda clase tiene una edad promedio de 29.5 años (mediana = 29) y 
                           la 1era clase tiene una edad promedio de 39.2 años (mediana = 39). Es decir, 
                           hay una relación creciente entre la edad del pasajero y la calidad de la clase. 
                           Este tipo de distribución tan centrada hace que haya pocos outliers por categoría (como puede verse en la figura).",
                           width = 100),
       caption = "Ignacio Spiousas") +
  theme_bw() +
  theme(plot.subtitle=element_text(size=10),
        plot.title.position = "plot")

ggsave(here("aprendizaje_estadistico/Pr_1/figs/cabina_edad.pdf"), width = 6.6, height = 6, units = "in")
