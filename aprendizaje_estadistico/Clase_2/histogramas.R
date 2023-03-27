library(datasets)
library(dplyr)
library(ggplot2)
library(magrittr)

data(airquality)

airquality %>%
  ggplot(aes(x = Ozone)) +
  geom_histogram() +
  theme_minimal()

h <- hist(airquality$Ozone)

h$counts
h$density

sum(h$counts)/(sum(h$counts)*diff(h$breaks))
sum(h$counts[1:2])/(sum(h$counts))

str(airquality)
airquality %>% ggplot(aes(x = Month)) + geom_bar()

par ( mfrow = c (1 , 2) )
hist( airquality$Ozone , main = " Hist . de frecuencias ")
hist( airquality$Ozone , main = " Histograma de densidad " , prob = TRUE )
par ( mfrow = c (1 , 1) )