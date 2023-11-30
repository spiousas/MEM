pacman::p_load(ISLR2, tidyverse, tree, patchwork)

arbol <- tree(Salary ~ Hits + Years, data = Hitters)
plot(arbol)
text(arbol, pretty = 0)
arbol

summary(arbol)

newdata <- crossing( Hits = seq(0, 200, 1), Years = 0:20 )

predict(arbol, 
        newdata = tibble(Hits = c(20, 100) , Years =c(5, 2)))

evaluated_grid <- newdata %>% 
  mutate(Salary = predict(arbol, newdata = newdata))

ggplot(evaluated_grid, aes(x = Years, y = Hits, fill = as.factor(round(Salary)))) + 
  geom_tile(alpha = .5) +
  geom_vline(xintercept = 4.5, linetype = "dashed") +
  geom_hline(yintercept = 39.5, linetype = "dashed") +
  geom_vline(xintercept = 3.5, linetype = "dashed") +
  geom_vline(xintercept = 6.5, linetype = "dashed") + 
  geom_hline(yintercept = 117.5, linetype = "dashed") +
  geom_vline(xintercept = 5.5, linetype = "dashed") + 
  geom_hline(yintercept = 185, linetype = "dashed") +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Salario predicho", fill = "Salario en miles de u$d") +
  theme_minimal() +
  theme(legend.position = "bottom")

set.seed(1234)
arbol <- tree(Salary ~ . , data = Hitters)
arbol_cv <- cv.tree(arbol, K = 5)

par(mfrow = c(1, 2))
results <- tibble(size = arbol_cv$size, dev = arbol_cv$dev, k = arbol_cv$k) 

p1 <- results %>%
  ggplot(aes(x = size,
             y = dev)) +
  geom_point() +
  geom_line(linewidth = 2, alpha = .5) +
  theme_bw()

p2 <- results %>%
  ggplot(aes(x = k,
             y = dev)) +
  geom_point() +
  geom_line(linewidth = 2, alpha = .5) +
  labs(caption = "k corresponde al alpha de validacion cruzada") +
  theme_bw()

p1 + p2

arbol_pruneado <- prune.tree(arbol, 
                             best = arbol_cv$size[which.min(arbol_cv$dev)] )
par(mfrow = c(1,1))
plot(arbol_pruneado)
text(arbol_pruneado, pretty = 0)