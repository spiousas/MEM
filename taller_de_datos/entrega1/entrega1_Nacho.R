pacman::p_load(tidyverse, here, glmnet)

data_x <- read_delim(here("taller_de_datos/entrega1/data/Vessel_X.txt"),
                     name_repair = "unique",
                     col_names = FALSE)

data_y <- read_delim(here("taller_de_datos/entrega1/data/Vessel_Y.txt"),
                     name_repair = "unique",
                     col_names = FALSE) %>%
  select(X4) %>%
  rename(Y = X4)

data_x_long <- data_x %>% 
  mutate(sample = factor(row_number())) %>%
  pivot_longer(cols = 1:last_col(1)) %>%
  rename(X = name) %>%
  mutate(X = parse_number(X) + 99) 

summ_data_x <- data_x_long %>%
  group_by(X) %>%
  summarise(m_value = mean(value),
            sem_value = sd(value)/sqrt(n()))

data_x_long %>%
  ggplot(aes(x = X, y = value)) +
  geom_line(aes(color = sample), alpha = .1) +
  geom_line(data = summ_data_x, aes(x = X, y = m_value), linewidth = 1) +
  scale_color_viridis_d() +
  theme_bw() +
  theme(legend.position = "none")

data_y %>%
  ggplot(aes(x = Y)) +
  geom_histogram() +
  theme_bw() +
  theme(legend.position = "none")
    
summ_data_x %>%
  ggplot(aes(x = X, y = sem_value)) +
  geom_line(linewidth = 1) +
  theme_bw()

data_xy <- data_x %>%
  bind_cols(data_y)
    
cor_coef <- rep(NA, 301)
for (i in 1:301) {
  cor_coef[i] <- lm(data_xy[,i], data_xy$Y)
}
plot(cor_coef)
lines(cor_coef)
cor(data_xy[,1], data_xy$Y)

tib_coeffs <- data_xy %>%
  pivot_wider(names_from = sample, values_from = )
  group_by(sample) %>% 
  nest() %>% 
  mutate(
    linMod = map(data, ~lm(data = ., Y ~ .)),
    coeffs = map(linMod, coefficients)
  )
tib_coeffs
# Se puede usar le p

set.seed(12)
cvfit <- cv.glmnet(as.matrix(data_x), as.matrix(data_y), alpha = 1)
plot(cvfit)
cvfit$lambda.min
cvfit$lambda.1se
sum(coef(cvfit, s = "lambda.1se")!=0)
sum(coef(cvfit, s = "lambda.min")!=0)

predict(cvfit, newx = as.matrix(data_x[1:5,]), s = "lambda.min")

set.seed(123)
cvfit <- cv.glmnet(as.matrix(data_x), as.matrix(data_y), type.measure = "mse", nfolds = 20, alpha = 0)
print(cvfit)$Measure[2]

alphas <- seq(0,1,.01)
errors <- rep(NA, length(alphas))
SEs <- rep(NA, length(alphas))

for (i in 1:length(alphas)) {
  cvfit <- cv.glmnet(as.matrix(data_x), as.matrix(data_y), type.measure = "mse", nfolds = 10, alpha = alphas[i])
  errors[i] <- cvfit$Measure[2]
  SEs[i] <- cvfit$SE[2]
}

results <- tibble(alphas, errors, SEs)

results %>%
  ggplot(aes(x = alphas,
             y = errors)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = errors-SEs, ymax = errors+SEs), alpha = .3) +
  theme_bw()
    

x <- as.matrix(data_x)
y <- as.matrix(data_y)
itrain <- sample(1:180, 180*.9)
fit <- glmnet(x[itrain, ], y[itrain], family = "gaussian")
assess.glmnet(fit, newx = x[-itrain, ], newy = y[-itrain])
