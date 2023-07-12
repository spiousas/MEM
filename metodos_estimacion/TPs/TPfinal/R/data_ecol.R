library(ash)
library(MASS)
library(here)
library(tidyverse)
library(janitor)
x <- rnorm(1000, 5, 3)
#x <- matrix( rnorm(700, sd = .1), 100 , 2)   
#x <- runif(500, 0, 10)
#y <- runif(500, 0, 10)
x_trim <- x[x>0 & x<10]
y <- runif(length(x_trim), 0, 10)
plot(y~x_trim)

bins <- bin2(cbind(x_trim, y), nbin = c(10,10))
bins
est_mv <- fitdistr(as.vector(bins$nc), densfun = "poisson")
x <- 1:max(as.vector(bins$nc))
dens <- dpois(x = x, lambda = est_mv$estimate[1])
hist(bins$nc, freq = F, ylim = c(0, .12))
lines(x, dens, col = "red")


est_mv <- fitdistr(as.vector(bins$nc), densfun = "negative binomial")
mean(as.vector(bins$nc))
var(as.vector(bins$nc))
est_mv$estimate
10+10^2/15
x <- 1:max(as.vector(bins$nc))
dens <- dnbinom(x = x, size = est_mv$estimate[1], mu = est_mv$estimate[2])
lines(x, dens, col = "blue")


especies <- read_delim(here("metodos_estimacion/TPs/TPfinal/R/data/ATLANTIC-PR_Occurrence.csv"), delim = ";") %>%
  clean_names()

especies %>% count(species)
especies %>% filter(species == "Alouatta guariba") %>% 
  ggplot(aes(x = longitude_x,
             y = latitude_y)) +
    geom_point()

x <- especies %>% filter(species == "Alouatta guariba") %>% pull(longitude_x)
y <- especies %>% filter(species == "Alouatta guariba") %>% pull(latitude_y)
plot(x~y)

bins <- bin2(cbind(x, y), nbin = c(10,10))
mean(as.vector(bins$nc))
var(as.vector(bins$nc))

est_mv_poi <- fitdistr(as.vector(bins$nc), densfun = "poisson")
est_mv_NB <- fitdistr(as.vector(bins$nc), densfun = "negative binomial")
x <- 1:max(as.vector(bins$nc))
dens_poi <- dpois(x = x, lambda = est_mv_poi$estimate[1])
dens_NB <- dnbinom(x = x, size = est_mv_NB$estimate[1], mu = est_mv_NB$estimate[2])
hist(bins$nc, freq = F, ylim = c(0, .1))
lines(x, dens_poi, col = "red")
lines(x, dens_NB, col = "blue")


data <- bins$nc[bins$nc!=0]
data

est_mv_poi <- fitdistr(data, densfun = "poisson")
est_mv_NB <- fitdistr(data, densfun = "negative binomial")
x <- 1:max(data)
dens_poi <- dpois(x = x, lambda = est_mv_poi$estimate[1])
dens_NB <- dnbinom(x = x, size = est_mv_NB$estimate[1], mu = est_mv_NB$estimate[2])
hist(data, freq = F, ylim = c(0, .05))
lines(x, dens_poi, col = "red")
lines(x, dens_NB, col = "blue")
 
library(sf)

data_geo <- especies %>% filter(species == "Alouatta guariba") %>% dplyr::select(all_of(c("longitude_x", "latitude_y")))
my_sf <- st_as_sf(data_geo, coords = c('longitude_x', 'latitude_y'))

#Plot it:

qmplot(longitude_x, latitude_y, data = data_geo, maptype = "toner-background", color = I("red")) +
  geom_hline(yintercept = 26)

ggplot(my_sf) + 
  geom_sf()

myLocation <- "Uruguay"

myMap <- get_map(location=myLocation,
                 source="stamen", maptype="watercolor", crop=FALSE)

mapdata <- map_data("world")

map1 <- mapdata %>%
  ggplot(aes(x = long,
           y = lat)) +
  geom_polygon()
map1  

library(leaflet)

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  setView(lng = mean(data_geo$longitude_x), lat = mean(data_geo$latitude_y)+2.5, zoom = 5) %>%
  addCircleMarkers(lng=data_geo$longitude_x, lat=data_geo$latitude_y,
                   stroke = FALSE, fillOpacity = 0.7, radius = 2
             )
m  # Print the map
