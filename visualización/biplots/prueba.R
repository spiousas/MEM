pacman::p_load(tidyverse, MASS, factoextra)

biplot_Spiousas <- function(X, V, lambdas, scale = 1, pc_biplot = F, isPCA = F) {
  # Plotea un biplot dados X (datos), V (Matriz de rotación) y lambdas (autovalores)
  #
  # pc_biplot: Si pc_biplot==T entonces divide los scores por sqrt(lambda_j) y
  # multiplica los vectores v_i scores por sqrt(lambda_j).
  #
  # scale: Simplemente alarga o acorta las flechas por un factor.
  #
  # isPCA: Es un booleano que si es PCA te pone el porcentaje de varianza explicada
  
  # Genero las Xs rotadas y la shago un tibble
  rotX <- as.matrix(X) %*% V
  colnames(rotX) <- c("PC1", "PC2")
  data <- as_tibble(rotX)
  
  # Hago un tibble con los vectores de rotación
  colnames(V) <- c("v1", "v2")
  V <- as_tibble(V)
  
  if (isPCA) {
    label <- list(
      labs(title = paste0("Biplot", if_else(pc_biplot, ", con los scores y v escaleados con lambda.", ".")),
           x = paste0("PC1 (", round(lambdas[1]/sum(lambdas)*100, 2), "%)"),
           y = paste0("PC2 (", round(lambdas[2]/sum(lambdas)*100, 2), "%)"))
    )  
  } else {
    label <- list(
      labs(title = paste0("Biplot", if_else(pc_biplot, ", con los scores y v escaleados con lambda.", ".")),
           x = "Componente 1",
           y = "Componente 2")
    ) 
  }
  
  
  # Si pc_biplot es T escaleo con lambdas, sino con 1s
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
    geom_segment(data = V,
                 aes(xend = scale * v1 * sqrt(lambda1), 
                     yend = scale * v2 * sqrt(lambda1)), 
                 x = 0, y = 0,
                 arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
                 color = "steelblue") +
    geom_text(data = V,
              aes(x = scale * v1 * sqrt(lambda1), 
                  y = scale * v2 * sqrt(lambda2)), 
              label = paste0("X", 1:nrow(V)),
              vjust = "outward", hjust = "outward") +
    label +
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

# Estos dos deberían dar iguales
pca_fun <- prcomp(Xsc)
pca_fun

fviz_pca_biplot(pca_fun,
                label = "var",
                col.ind = "grey80",
                size.ind = .1,
                ggtheme = theme_bw())

# Doy vuelta la primera columna de A
biplot_Spiousas(Xsc, cbind(-A[,1], A[,2]), eigvalues, scale = 3.4, pc_biplot = T, isPCA = F)

