pacman::p_load(tidyverse, here, multcompView, performance)

data_veneno <- read_csv(here("taller_de_datos/entrega0/data/problema0.csv")) %>%
  mutate(tratamiento = as.factor(tratamiento),
         veneno = as.factor(veneno))
summary(data_veneno)

data_veneno %>% 
  ggplot(aes(x = tratamiento,
             y = sobrevida)) +
  geom_boxplot(outlier.shape = NA, color = "steelblue") +
  geom_jitter(width = .1) +
  theme_bw() 

data_veneno %>% 
  ggplot(aes(x = veneno,
             y = sobrevida,
             color = tratamiento)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_dodge(width = .73)) +
  theme_bw() +
  scale_colour_brewer(palette = "Dark2") +
  theme(legend.position = "top")
  
m1 <- lm(data = data_veneno, sobrevida ~ veneno * tratamiento)
summary(m1)
check_model(m1)

an.m1 <- aov(m1)
tukey.m1 <- TukeyHSD(x = an.m1, "veneno", conf.level = 0.95)
tukey.m1
