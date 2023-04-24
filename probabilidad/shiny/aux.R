pxy <- matrix(runif(n = 18, min = 0, max = 1), nrow = 2, ncol = 9)
pxy <- pxy/sum(pxy)
sum(pxy)
colnames(pxy) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9")

g <- rbinom(n = 9, size = 1, prob = .5)
g

sum(pxy[1,g!=0]) + sum(pxy[2,g!=1])

n <- 9
l <- rep(list(0:1), n)

gs <- expand.grid(l)
colnames(gs) <- paste0("X", 1:9)
gs
error <- c()
for (i in 1:nrow(gs)) {
  error[i] <- sum(pxy[1,gs[i,]!=0]) + sum(pxy[2,gs[i,]!=1])
}

errors <- error
error_tbl <- as_tibble(gs) %>% 
  bind_cols(tibble(error)) %>%
  arrange(desc(error)) %>%
  mutate(order = row_number())

gs_tbl <- error_tbl %>% 
  pivot_longer(
    cols = starts_with("X"),
    names_to = "X",
    values_to = "value",
    values_drop_na = TRUE
  ) %>%
  mutate(X = (parse_number(X)-1)/8*(max(error)-min(error)) + min(error))

ggplot() +
  geom_tile(data = gs_tbl,
            aes(x=order, y = X, fill = factor(value)),
            alpha = .4) +
  geom_line(data = error_tbl,
             aes(x=order, y = error),
            size = 2) +
  scale_fill_manual(values = c("red", "green")) +
  labs(x = "Orden de mayor a menor error",
      y = "Error de clasificación",
      fill = "Estructura de la g(x)") +
  theme_bw() +
  theme(legend.position = "top")
  
g <- gs[which.min(error),]
sum(pxy[1,g!=0]) + sum(pxy[2,g!=1])
plot(error)
lines(error)
plot(sort(error))
lines(error)
