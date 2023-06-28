# Ejercicio 7 ####
estimar_p_intervalo <- function(nivel, x, metodo = 1) {
  alfa <- 1 - nivel
  Z <- qnorm(1-alfa/2)
  n <- length(x)
  if (metodo == 1) {
    int <- c(mean(x) - Z * sqrt(mean(x)*(1-mean(x))/n), 
             mean(x) + Z * sqrt(mean(x)*(1-mean(x))/n))  
  } else {
    a <- n^2 + n*Z^2
    b <- -2*n*sum(x) - Z^2*n
    c <- (sum(x))^2
    int <- c((-b - sqrt(b^2-4*a*c)) / (2*a), 
             (-b + sqrt(b^2-4*a*c)) / (2*a)) 
  }
  int
}
x <- rbinom(prob = .5, n = 100000, size = 1)
estimar_p_intervalo(.95, x, metodo = 2)

hallar_n_p <- function(nivel, lmax) {
  alfa <- 1 - nivel
  ceiling((qnorm(1-alfa/2) / lmax)^2) 
}
hallar_n_p(.95, 0.1)

# Ejercicio 8 ####
calcular_cubrimiento_empirico <- function(p, nivel, n, Nrep, semilla, metodo = 1) {
  cubrimiento <- rep(NA, Nrep)
  long <- rep(NA, Nrep)
  set.seed(semilla)
  for (i in 1:Nrep) {
    x <- rbinom(prob = p, n = n, size = 1)
    int <- estimar_p_intervalo(nivel, x, metodo)
    cubrimiento[i] <- (p>=int[1]) & (p<=int[2])
    long[i] <- diff(int)
  } 
  paste0(round(mean(cubrimiento), digits = 3)," (", round(mean(long), digits = 3), ")")
}

calcular_cubrimiento_empirico(p = .5, nivel = .95, n = 100, Nrep = 1000, semilla = 2)

data_1 <- expand_grid(n = c(5, 10, 30, 50, 100, 1000),
                      p = c(0.1, 0.5)) %>%
  rowwise() %>%
  mutate(result = calcular_cubrimiento_empirico(p = p, nivel = .95, n = n, Nrep = 1000, semilla = 2, metodo = 1)) %>%
  pivot_wider(names_from = n, values_from = result) 

data_1 %>%
  kbl(caption = "Metodo 1", # Adding caption  
      format = "latex") %>% # Output format = latex 
  kable_classic(html_font = "Cambria") # Font = Cambria 

data_2 <- expand_grid(n = c(5, 10, 30, 50, 100, 1000),
                      p = c(0.1, 0.5)) %>%
  rowwise() %>%
  mutate(result = calcular_cubrimiento_empirico(p = p, nivel = .95, n = n, Nrep = 1000, semilla = 2, metodo = 2)) %>%
  pivot_wider(names_from = n, values_from = result) 

data_2 %>%
  kbl(caption = "Metodo 2", # Adding caption  
      format = "latex") %>% # Output format = latex 
  kable_classic(html_font = "Cambria") # Font = Cambria 

