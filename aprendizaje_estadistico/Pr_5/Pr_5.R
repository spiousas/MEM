pacman::p_load(tidyverse, here)

# 8 ####
data <- read_csv(here("aprendizaje_estadistico/Pr_5/alturas_n_490.csv")) %>%
  select(c(altura, genero)) %>%
  mutate(genero_num = if_else(genero == "F", 0, 1))
str(data)

class_knn <- function(x, y, x_nuevo, k) {
  as.numeric(sum(y[order(abs(x-x_nuevo))[1:k]])/k > 0.5)
}

loocv_knn <- function(k) {
  CV <- rep(NA, nrow(data))
  for (i in (1:length(CV))) {
    pred_i <- class_knn(x = data$altura[-i],
                        y = data$genero_num[-i],
                        x_nuevo = data$altura[i],
                        k = k)
    CV[i] <- (data$genero_num[i] - pred_i)^2
  }
  mean(CV)
}

k_seq <- 3:20
CV_k <- rep(NA, length(k_seq))
for (j in 1:length(k_seq)) {
  CV_k[j] <- loocv_knn(k_seq[j])
}

# Me quedo con el k más grande
k_opt <- max(k_seq[CV_k == min(CV_k)])
k_opt

CV_k[k_seq == 10]
CV_k[k_seq == k_opt]

plot(k_seq, CV_k)

# 9 ####

class_prop_loc <- function(x, y, x_nuevo, h) {
  as.numeric(sum(y[abs(x-x_nuevo)<h]) / sum(abs(x-x_nuevo)<h) > 0.5)
}

loocv_prop_loc <- function(h) {
  CV <- rep(NA, nrow(data))
  for (i in (1:length(CV))) {
    pred_i <- class_prop_loc(x = data$altura[-i],
                             y = data$genero_num[-i],
                             x_nuevo = data$altura[i],
                             h = h)
    CV[i] <- (data$genero_num[i] - pred_i)^2
  }
  mean(CV, na.rm = TRUE)
}

h_seq <- seq(1.5, 12, 0.05)
CV_h <- rep(NA, length(h_seq))
for (j in 1:length(h_seq)) {
  CV_h[j] <- loocv_prop_loc(h_seq[j])
}

min(CV_h)

h_opt <- max(h_seq[CV_h == min(CV_h)])

CV_h[h_seq == 1.5]
CV_h[h_seq == h_opt]

plot(h_seq, CV_h)

# 10 ####
class_gen <- function(x, y, x_nuevo, h0, h1) {
  P_x_0 <- density(x[y == 0], kernel = "gaussian", from = x_nuevo, to = x_nuevo, n =1, bw = h0)$y
  prop_x_0 <- mean(y == 0)
  
  P_x_1 <- density(x[y == 1], kernel = "gaussian", from = x_nuevo, to = x_nuevo, n =1, bw = h1)$y
  prop_x_1 <- mean(y == 1)
  
  as.numeric(P_x_1 * prop_x_1 > P_x_0 * prop_x_0)
}

loocv_gen <- function(h_0, h_1) {
  CV <- rep(NA, nrow(data))
  for (i in (1:length(CV))) {
    pred_i <- class_gen(x = data$altura[-i],
                        y = data$genero_num[-i], 
                        x_nuevo = data$altura[i],
                        h0 = h_0,
                        h1 = h_1)
    CV[i] <- (data$genero_num[i] - pred_i)^2
  }
  mean(CV)
}

h_0_seq <- seq(1, 10, .5)
h_1_seq <- seq(1, 10, .5)
CV_h2 <- matrix(, nrow = length(h_0_seq), ncol = length(h_1_seq))
for (j in 1:length(h_0_seq)) {
  for (k in 1:length(h_1_seq)) {
    print(paste("j =", j, "- k =", k))
    CV_h2[j,k] <- loocv_gen(h_0_seq[j], h_1_seq[k])
  }
}

optim_hs <- which(CV_h2 == min(CV_h2), arr.ind=TRUE)

h_0_opt <- h_0_seq[optim_hs[which.max(h_0_seq[optim_hs[,1]] + h_1_seq[optim_hs[,2]]),1]]
h_1_opt <- h_1_seq[optim_hs[which.max(h_0_seq[optim_hs[,1]] + h_1_seq[optim_hs[,2]]),2]]
h_0_opt
h_1_opt

# 11 ####
data_test <- read_csv(here("aprendizaje_estadistico/Pr_5/alturas_testeo.csv")) %>%
  select(c(altura, genero)) %>%
  mutate(genero_num = if_else(genero == "F", 0, 1))
str(data_test)

errores <- data_test %>% 
  rowwise() %>%
  mutate(knn = class_knn(data$altura, data$genero_num, altura, k_opt),
         prop_loc = class_prop_loc(data$altura, data$genero_num, altura, h_opt),
         gen = class_gen(data$altura, data$genero_num, altura, h_0_opt, h_1_opt),
         error_knn = sum(as.numeric(genero_num!=knn)),
         error_prop_loc = sum(as.numeric(genero_num!=prop_loc)),
         error_gen = sum(as.numeric(genero_num!=gen))) %>%
  ungroup()

errores %>% 
  select(c(error_knn, error_prop_loc, error_gen)) %>%
  summarise_all(mean)

# Clasifica mejor KNN

# 12 ####

data_xNuevo <- tibble(xnuevo = seq(160, 170, .01)) %>%
  rowwise() %>%
  mutate(knn = class_knn(data$altura, data$genero_num, xnuevo, k_opt),
         prop_loc = class_prop_loc(data$altura, data$genero_num, xnuevo, h_opt),
         gen = class_gen(data$altura, data$genero_num, xnuevo, h_0_opt, h_1_opt))

data_xNuevo %>% 
  pivot_longer(cols = !xnuevo, names_to = "Metodo", values_to = "Prediccion") %>%
  ggplot(aes(x = xnuevo,
             y = Prediccion, 
             color = Metodo)) +
  geom_point() +
  geom_line() +
  labs(x = "xNuevo", 
       y = "Clasificación con método generativo (0=F y 1=M)",
       title = "El punto de corte está entre 164.45 y 144.46 cm") +
  scale_y_continuous(breaks = c(0,1)) +
  theme_bw()

# Para la entrega
data_xNuevo %>%
  ggplot(aes(x = xnuevo,
             y = gen)) +
  geom_point() +
  geom_line() +
  labs(x = "xNuevo", 
       y = "Clasificación con método generativo (0=F y 1=M)",
       title = "El punto de corte está entre 164.45 y 144.46 cm") +
  scale_y_continuous(breaks = c(0,1)) +
  theme_bw()
    
ggsave(here("aprendizaje_estadistico/Pr_5/Spiousas.pdf"))

  