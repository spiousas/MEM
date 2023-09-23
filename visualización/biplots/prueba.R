pacman::p_load(tidyverse, MASS)

biplot_nacho <- function(X, V, lambdas, scale = 1, pc_biplot = F) {
  
  rotX <- as.matrix(X) %*% V
  colnames(rotX) <- c("PC1", "PC2")
  data <- as_tibble(rotX)
  
  if (pc_biplot) {
    lambda1 <- lambdas[1]
    lambda2 <- lambdas[2]
  } else {
    lambda1 <- 1
    lambda2 <- 1
  }
  
  data %>% ggplot(aes(x = PC1/sqrt(lambda1),
                      y = PC2/sqrt(lambda2))) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_point(color = "gray80", alpha = .8) +
    geom_segment(data = as_tibble(A),
                 aes(xend = scale * V1 * sqrt(lambda1), 
                     yend = scale * V2 * sqrt(lambda2)), 
                 x = 0, y = 0,
                 arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
                 color = "steelblue") +
    geom_text(data = as_tibble(A),
              aes(x = scale * V1 * sqrt(lambda1), 
                  y = scale * V2 * sqrt(lambda2)), 
              label = paste0("X", 1:nrow(A)),
              vjust = "outward", hjust = "outward") +
    labs(title = "Biplot Nacho",
         x = paste0("PC1 (", round(lambdas[1]/sum(lambdas)*100, 2), "%)"),
         y = paste0("PC2 (", round(lambdas[2]/sum(lambdas)*100, 2), "%)")) +
    theme_bw()
}

# Simulo datos ####
n <- 500
mu <- c(1/2, 1, 2)
sigma <- matrix(c(1,1/2,1/4,1/2,2,1/3,1/4,1/3,3),3)
set.seed(1234)
X <- mvrnorm(n, mu, sigma)

Xsc <- scale(X)

A <- eigen(cov(as.matrix(Xsc)))$vectors[,1:2]
eigvalues <- eigen(cov(as.matrix(Xsc)))$values

biplot_nacho(Xsc, A, eigvalues, scale = 3.4, pc_biplot = F)
  