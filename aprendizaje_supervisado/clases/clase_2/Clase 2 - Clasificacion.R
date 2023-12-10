pacman::p_load(FNN, ISLR2, tidyverse, patchwork)

# Los datos de default de la tarjeta de crédito ####
head(Default)
summary(Default)

# Algunas figuritas descriptivas ####
ggplot(Default, aes(x = balance, y = income, color = default, pch =student)) +
  geom_point(alpha = .5) + 
  ggtitle('Default data set') +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot(Default, aes(x = default , y = balance)) +
  geom_jitter(width = .2, alpha = .1) +
  geom_boxplot(alpha = 0.1, color = "steelblue", fill = "steelblue") +
  ggtitle('Distribucion del balance por default') +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot(Default, aes(x = default , y = balance)) +
  geom_jitter(width = .2, alpha = .1) +
  geom_boxplot(alpha = 0.1, color = "steelblue", fill = "steelblue") +  
  ggtitle('Distribucion del balance por default y estudiante') + 
  facet_wrap( ~ student) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Ajuste de modelos ####
# Armamos un dataframe con los valores donde queremos evaluar el clasificador

vals  = crossing(balance = seq(min(Default$balance), max(Default$balance), length.out = 51), 
                 income = seq(min(Default$income), max(Default$income), length.out = 51))

# Vecinos cercanos con diferentes Ks
# Las categorías
vals['knn10'] = knn(train = Default %>% select(balance, income), 
                  test = vals %>% select(balance, income) , 
                  cl = Default %>% pull(default), k = 10, prob = TRUE)
# Las probas
vals['knn10_prob'] =1 - attributes( knn(train = Default %>% select(balance, income), 
                    test = vals %>% select(balance, income) , 
                    cl = Default %>% pull(default), k = 10, prob = TRUE) )$prob

vals['knn3'] = knn(train = Default %>% select(balance, income), 
                    test = vals %>% select(balance, income) , 
                    cl = Default %>% pull(default), k = 3, prob = TRUE)

vals['knn3_prob'] =1- attributes( knn(train = Default %>% select(balance, income), 
                   test = vals %>% select(balance, income) , 
                   cl = Default %>% pull(default), k = 3, prob = TRUE))$prob

# glm con dos predictoras (aditivo)
fit1 = glm(default ~ balance + income , data = Default, family = binomial)

vals['proba'] = predict( fit1 , newdata =  vals %>% select(balance, income), type = 'response')
vals['logistic'] = ifelse( predict( fit1 , newdata =  vals %>% select(balance, income), type = 'response') > 0.5, 'Yes', 'No')

# Graficamos los resultados ####
# Las regiones predichas
p1 = ggplot(vals, aes( x = balance, y = income , fill = knn3)) + 
  geom_tile(alpha = .7) +
  ggtitle('Regiones de clasificacion K = 3') +
  theme_bw() +
  theme(legend.position = "bottom")

p2 = ggplot(vals, aes( x = balance, y = income , fill = knn10)) + 
  geom_tile(alpha = .7) +
  ggtitle('Regiones de clasificacion K = 10') +
  theme_bw() +
  theme(legend.position = "bottom")
p1 + p2

# Las regiones predichas con las etiquetas reales
p1 = ggplot(vals, aes( x = balance, y = income)) + 
  geom_tile(aes(fill = knn3), alpha = .4) +
  geom_point(data = Default, aes(x = balance, y = income, fill = default), pch = 21, alpha = 1, size = 1.5) +
  ggtitle('Regiones de clasificacion K = 3') +
  theme_bw() +
  theme(legend.position = "bottom")

p2 = ggplot(vals, aes( x = balance, y = income)) + 
  geom_tile(aes(fill = knn10), alpha = .4) +
  geom_point(data = Default, aes(x = balance, y = income, fill = default), pch = 21, alpha = 1, size = 1.5) +
  ggtitle('Regiones de clasificacion K = 10') +
  theme_bw() +
  theme(legend.position = "bottom")
p1 + p2

p3 = ggplot(vals, aes( x = balance, y = income)) + 
  geom_tile(aes(fill = logistic), alpha = .4) +
  ggtitle('Probabilidad de default') +
  theme_bw() +
  theme(legend.position = "bottom")

p4 = ggplot(vals, aes( x = balance, y = income)) + 
  geom_tile(aes(fill = logistic), alpha = .4) +
  geom_point(data = Default, aes(x = balance, y = income, fill = default), pch = 21, alpha = 1, size = 1.5) +
  ggtitle('Probabilidad de default') +
  theme_bw() +
  theme(legend.position = "bottom")
p3+p4

ggplot(vals, aes( x = balance, y = income , fill = proba)) + 
  geom_tile() +
  ggtitle('Logistica') +  
  scale_color_gradient(limits = c(0, 1)) +   
  theme_bw() +
  theme(legend.position = "none") +

ggplot(vals, aes( x = balance, y = income , fill = knn10_prob)) +
  geom_tile() +
  ggtitle('Knn 10') +  
  scale_color_gradient(limits = c(0, 1)) +   
  labs(fill ="Proba\ndefault") +
  theme_bw() +
  theme(legend.position = "bottom")+

ggplot(vals, aes( x = balance, y = income , fill = knn3_prob)) +
  geom_tile() +
  ggtitle('Knn3') +  
  scale_color_gradient(limits = c(0, 1)) +   
  theme_bw() +
  theme(legend.position = "none")

# Ajustando GLM ####
fit1 = glm(default ~ balance + income , data = Default, family = binomial)
fit2 = glm(default ~ balance + income + student, data = Default, family = binomial)
fit3 = glm(default ~ balance + income*student, data = Default, family = binomial)

summary(fit1)
summary(fit2)
summary(fit3)

vals['probs'] = predict(fit1, newdata = vals, type = 'response')
vals = vals %>% mutate(class05 = ifelse( probs > 0.5, 'Yes', 'No' ), 
                       class09 = ifelse( probs > 0.9, 'Yes', 'No' ), 
                       class01 =ifelse( probs > 0.1, 'Yes', 'No' ) )
