pacman::p_load(ISLR2, tibble, ggplot2, dplyr, patchwork, tidyr)

head(Auto)

ggplot(Auto, aes( x = horsepower, y = mpg )) + 
  geom_smooth(linewidth = 2, alpha = .2, se = F) +
  geom_point() + 
  ggtitle('Millas por galon vs Caballos de fuerza') +
  theme_bw()

####
# Como separar una muestra en dos grupos aleatorios de igual tamaño_
#### Partir la muestra en 2 grupos, uno para ajustar y el otro para validar

set.seed(1234)
train_split <- sample(1:nrow(Auto) %% 2, 
                      replace = FALSE ) 
p1 <- ggplot(Auto , aes(x = horsepower, y = mpg, color = ifelse(train_split, 'validation', 'train') ) ) + 
  geom_point() + 
  scale_color_brewer(palette = "Dark2", name = "Grupo") + 
  ggtitle('Separacion en Validacion y Entrenamiento') +
  theme_bw() +
  theme(legend.position = "bottom")
p1

train <- Auto[ train_split == 0,  ]
test <- Auto[ train_split == 1,  ]

train_error <- rep(NA, 10)
test_error <- rep(NA, 10)

for (p in 1:10){
   fit <- lm( mpg ~ poly(horsepower, degree = p), data = train ) #
   test_error[p] <- mean( (test$mpg - predict(fit, newdata = test) )^2 ) # El MSE
   train_error[p] <- mean( fit$residuals^2 )
}

p2 <- tibble(p = 1:10, test = test_error, train = train_error) %>%
  pivot_longer(test:train, names_to = "Datos", values_to = "value") %>%
  ggplot(aes(x = p, y = value, color = Datos)) + 
  geom_point(size = 2) +
  geom_line(alpha = 0.5, linewidth = 2) + 
  xlab( 'Grado del polinomio  mpg ~ 1 + hp + ...  hp^p') + 
  ggtitle('MSE') +
  scale_color_brewer(palette = "Paired", name = "Datos") + 
  theme_bw() +
  theme(legend.position = "bottom")

p1 + p2

###
# Distintas particiones de entrenamiento y validacion

p1 <- ggplot(Auto , aes(x = horsepower, y = mpg, color = ifelse(sample( 1:nrow(Auto)%% 2, replace = FALSE ), 'validation', 'train') ) ) + 
  geom_point() + 
  theme_bw() +
  scale_color_brewer(palette = "Dark2", name = "Grupo") + 
  theme(legend.position="none") +
  xlab("")

p2 <- ggplot(Auto , aes(x = horsepower, y = mpg, color = ifelse(sample( 1:nrow(Auto)%% 2, replace = FALSE ), 'validation', 'train') ) ) + 
  geom_point() + 
  theme_bw() +
  scale_color_brewer(palette = "Dark2", name = "Grupo") + 
  theme(legend.position="none") + 
  xlab("") + 
  ylab("")

p3 <- ggplot(Auto , aes(x = horsepower, y = mpg, color = ifelse(sample( 1:nrow(Auto)%% 2, replace = FALSE ), 'validation', 'train') ) ) + 
  geom_point() +
  theme_bw() +
  scale_color_brewer(palette = "Dark2", name = "Grupo") + 
  theme(legend.position="none") 

p4 <- ggplot(Auto , aes(x = horsepower, y = mpg, color = ifelse(sample( 1:nrow(Auto)%% 2, replace = FALSE ), 'validation', 'train') ) ) + 
  geom_point() +
  theme_bw() +
  scale_color_brewer(palette = "Dark2", name = "Grupo") + 
  ylab("") +
  theme(legend.position="bottom") 

p1 + p2 + p3 + p4 +
  plot_annotation('Distintas particiones en validacion y entrenamiento')

K <- 4
data <- Auto %>% 
  mutate(CC = sample( 1:nrow(Auto)%%K  +1 ),
         C = as.factor(CC))
pp1 <- ggplot(data, aes(x =horsepower, y = mpg, color = C)) +
  geom_point() +
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  xlab("") +
  ylab("") +
  ggtitle('Validación cruzada K = 4 - Partición en grupos')
pp1

k <- 1
pp2 <- ggplot(data, aes(x =horsepower, y = mpg, color = (CC == 1) )) + 
  geom_point() +
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position="none") +
  xlab("") +
  ylab("") +
  ggtitle(paste( 'k =', k))
pp2

k <- k + 1
pp3 <- ggplot(data , aes(x =horsepower, y = mpg, color = ifelse( CC == 2, 'Validation', 'Train' ))) +
  geom_point() + 
  xlab("") +
  ylab("") +
  ggtitle(paste( 'k =', k)) +
  scale_color_brewer(palette = "Dark2", name = "Esquema") +
  theme_bw()
pp3

k <- k + 1
pp4 <- ggplot(data, aes(x =horsepower, y = mpg, color = (CC == 3) )) +
  geom_point() +
  theme_bw() +
  scale_color_brewer(palette = "Dark2", name = "Esquema") +
  theme(legend.position="none") + 
  xlab("") +
  ylab("") +
  ggtitle(paste( 'k =', k))
pp4

k <- k + 1
pp5 <-  ggplot(data, aes(x =horsepower, y = mpg, color = ifelse( CC == 4, 'Validation', 'Train' ))) +
  geom_point() +
  theme_bw() +
  theme(legend.position="none") +
  scale_color_brewer(palette = "Dark2", name = "Esquema") +
  xlab("") +
  ylab("") +
  ggtitle(paste( 'k =', k))
pp5

(pp1 ) /( pp2 + pp3 ) /(pp4 + pp5) 

### Ejercicio alta correlacion (ayudita)

X <- matrix( rnorm(1000*50), nrow = 50)
p <- 1/(1+exp(-rowSums(2*X[, 1:7]))) #logit fun

Y <- rbinom(50, 1, p)

cors_val <- abs( cor(X, Y) )
hist(cors_val)
k <- 5 

(max_cor_indx = which( cors_val > sort(cors_val, decreasing = TRUE)[k] ))

sel_X = X[, max_cor_indx]

#### Stepwise
M0 <- lm(crim ~ 1, data = Boston)
Mp <- formula(lm( crim ~ . , data = Boston))

step(M0, scope = Mp, direction ='forward')
step(M0, scope = Mp, direction ='both')
