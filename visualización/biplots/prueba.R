pacman::p_load(tidyverse, MASS, factoextra)

biplot_Spiousas <- function(X, V, lambdas, scale = 1, pc_biplot = F, method = NULL, group_color = NULL,
                            ellipse = F) {
  pacman::p_load(ggplot2, ggforce)
  # Plotea un biplot dados X (datos), V (Matriz de rotación) y lambdas (autovalores)
  #
  # pc_biplot: Si pc_biplot==T entonces divide los scores por sqrt(lambda_j) y
  # multiplica los vectores v_i scores por sqrt(lambda_j).
  #
  # scale: Simplemente alarga o acorta las flechas por un factor.
  #
  # method: Puede ser "pca" o "cd" para plotear cosas específicas de PCA o 
  # coordenadas discriminantes, respectivamente.
  #
  # group_color: Pone el color de acuerdo al array que  venga ahí.
  #
  # ellipse: Plotea la elipse de los datos (asumiendo gaussiana). Util para CD.
  
  
  # Genero las Xs rotadas y la shago un tibble
  rotX <- as.matrix(X) %*% as.matrix(V)
  colnames(rotX) <- c("C1", "C2")
  data <- as_tibble(rotX)
  
  # Labels de las direcciones
  if (is.null(rownames(V))) {
    labels_V <- paste0("X", 1:nrow(V))
  } else {
    labels_V <- rownames(V)
  }
  
  # Hago un tibble con los vectores de rotación
  colnames(V) <- c("v1", "v2")
  V <- as_tibble(V)
  
  if (is.null(method)) {
    labels <- list(
      labs(x = "Componente 1",
           y = "Componente 2")
    ) 
  } else if (method == "pca") {
    labels <- list(
      labs(x = paste0("PC1 (", round(lambdas[1]/sum(lambdas)*100, 2), "%)"),
           y = paste0("PC2 (", round(lambdas[2]/sum(lambdas)*100, 2), "%)"))
    )  
  } else if (method == "cd") {
    labels <- list(
      labs(x = paste0("LD1 (", round(lambdas[1]/sum(lambdas)*100, 2), "%)"),
           y = paste0("LD2 (", round(lambdas[2]/sum(lambdas)*100, 2), "%)"))
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
  
  if (is.null(group_color)) {
    puntos <- geom_point(color = "gray80", alpha = .8)
  } else {
    puntos <- geom_point(aes(color = group_color), alpha = .8)
  }
  
  if (ellipse & !is.null(group_color)) {
    elipse <- stat_ellipse(aes(color = group_color, fill = group_color), 
                           geom = "polygon", alpha = .2)
  } else {
    elipse <- {}
  }
  
  data %>% ggplot(aes(x = C1/sqrt(lambda1),
                      y = C2/sqrt(lambda2))) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    elipse +
    puntos +
    geom_segment(data = V,
                 aes(xend = scale * v1 * sqrt(lambda1), 
                     yend = scale * v2 * sqrt(lambda1)), 
                 x = 0, y = 0,
                 arrow = arrow(length = unit(0.03, "npc"), type = "closed"),
                 color = "steelblue") +
    geom_text(data = V,
              aes(x = scale * v1 * sqrt(lambda1), 
                  y = scale * v2 * sqrt(lambda2)), 
              label = labels_V,
              vjust = "outward", hjust = "outward") +
    labs(title = paste0("Biplot", if_else(pc_biplot, ", con los scores y v escaleados con lambda.", ".")),
         color = "Grupo") +
    labels +
    guides(fill = "none") +
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

# Estos dos deberían dar iguales
pca_fun <- prcomp(Xsc)
pca_fun

fviz_pca_biplot(pca_fun,
                label = "var",
                col.ind = "grey80",
                size.ind = .1,
                ggtheme = theme_bw())

# Doy vuelta la primera columna de A
biplot_Spiousas(Xsc, cbind(-A[,1], A[,2]), eigvalues, scale = 3.4, pc_biplot = F, method = "pca")

biplot(Xsc %*% cbind(-A[,1], A[,2]), cbind(-A[,1], A[,2]), xlabs = rep("·", nrow(Xsc)), ylabs = rep(".", ncol(Xsc)))
