pacman::p_load(tidyverse, here, multcompView, performance, MASS)

# Carga de los datos ####
data_veneno <- read_csv(here("taller_de_datos/entrega0/data/problema0.csv")) %>%
  mutate(tratamiento = as.factor(tratamiento),
         veneno = as.factor(veneno),
         log_sobrevida = log(sobrevida))
summary(data_veneno)

# Visualización previa ####
## Boxplots ####
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

## Intercation plots ####
data_veneno %>%
  group_by(veneno, tratamiento) %>%
  summarise(m_sobrevida = mean(sobrevida)) %>%
  ggplot(aes(x = veneno,
             y = m_sobrevida,
             color = tratamiento,
             group = tratamiento)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_colour_brewer(palette = "Dark2") +
  theme(legend.position = "top")

data_veneno %>%
  group_by(veneno, tratamiento) %>%
  summarise(m_sobrevida = mean(sobrevida)) %>%
  ggplot(aes(x = tratamiento,
             y = m_sobrevida,
             color = veneno,
             group = veneno)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_colour_brewer(palette = "Dark2") +
  theme(legend.position = "top")
    
# Modelo aditivo ####
m_aditivo <- lm(data = data_veneno, sobrevida ~ veneno + tratamiento)
summary(m_aditivo)

## Supuestos ####
check_model(m_aditivo)

## Anova ####
an.m_aditivo <- aov(m_aditivo)
summary(an.m_aditivo)

## Tukey ####
tukey.m_aditivo <- TukeyHSD(x = an.m_aditivo, "tratamiento", conf.level = 0.95, ordered = T)
tukey.m_aditivo
plot(tukey.m_aditivo)

# Modelo aditivo log ####
m_aditivo_log <- lm(data = data_veneno, log_sobrevida ~ veneno + tratamiento)
summary(m_aditivo_log)

## Supuestos ####
check_model(m_aditivo_log)

## Anova ####
an.m_aditivo_log <- aov(m_aditivo_log)
summary(an.m_aditivo_log)

## Tukey ####
tukey.m_aditivo_log <- TukeyHSD(x = an.m_aditivo_log, "tratamiento", conf.level = 0.95, ordered = T)
tukey.m_aditivo_log
plot(tukey.m_aditivo_log)

# Modelo aditivo boxcox ####
best_transf <- boxcox(m_aditivo)
lambda <- best_transf$x[which.max(best_transf$y)]

data_veneno <- data_veneno %>%
  mutate(boxcox_sobrevida = (sobrevida^lambda-1)/lambda)

m_aditivo_boxcox <- lm(data = data_veneno, boxcox_sobrevida ~ veneno + tratamiento)
summary(m_aditivo_boxcox)

## Supuestos ####
check_model(m_aditivo_boxcox)

## Anova ####
an.m_aditivo_boxcox <- aov(m_aditivo_boxcox)
summary(an.m_aditivo_boxcox)

## Tukey ####
tukey.m_aditivo_boxcox <- TukeyHSD(x = an.m_aditivo_boxcox, "tratamiento", conf.level = 0.95, ordered = T)
tukey.m_aditivo_boxcox
plot(tukey.m_aditivo_boxcox)

# Modelo con interacción  ####
m_interact <- lm(data = data_veneno, sobrevida ~ veneno * tratamiento)
summary(m_interact)

## Supuestos ####
check_model(m_interact)

## Anova ####
an.m_interact <- aov(m_interact)
summary(m_interact)

## Tukey ####
tukey.m_interact <- TukeyHSD(x = an.m_interact, "tratamiento", conf.level = 0.95, ordered = T)
tukey.m_interact
plot(tukey.m_interact