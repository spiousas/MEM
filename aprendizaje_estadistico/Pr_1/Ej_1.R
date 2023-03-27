library(datasets)
library(tidyverse)

# 1 ####
count(airquality)

# 2 ####
names(airquality)
colnames(airquality)
  
# 3 ####
str(airquality)

# 4 ####
airquality %>%
  select(everything()) %>%
  summarise_all(~ (sum(is.na(.))))

# 5 ####
airquality %>%
  group_by(Month) %>%
  count()

# 6 ####
airquality %>% ggplot(aes(x = Wind)) +
  geom_density() +
  geom_histogram(aes(y = ..density..), alpha = .2) +
  theme_minimal()

airquality %>% ggplot(aes(x = Temp)) +
  geom_density() +
  geom_histogram(aes(y = ..density..), alpha = .2) +
  theme_minimal()

airquality %>% filter(between(Temp, 65, 70)) %>% count()/count(airquality)
airquality %>% filter(between(Temp, 65, 70)) %>% count()

# 7 ####
find_outlier <- function(x) {
  return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
}

airquality_outliers <- airquality %>%
  mutate(outlier_Wind = ifelse(find_outlier(Wind), paste0("Dia ",Day, " - Mes", Month), NA))

airquality_outliers <- airquality_outliers %>%
  mutate(outlier_Temp = ifelse(find_outlier(Temp), paste0("Dia ",Day, " - Mes", Month), NA))

airquality_outliers %>% ggplot(aes(y = Wind)) +
  stat_boxplot() +
  geom_text(aes(label=outlier_Wind), x = 0, na.rm=TRUE, hjust=-0.2) +
  theme_minimal()

airquality_outliers %>% ggplot(aes(y = Temp)) +
  stat_boxplot() +
  geom_text(aes(label=outlier_Temp), x = 0, na.rm=TRUE, hjust=-0.2) +
  theme_minimal()

# 8 ####
airquality %>% ggplot(aes(x = Wind, y = Temp)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_minimal()
