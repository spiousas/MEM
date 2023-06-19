# Ejercicio 2 ####
datos <- c(16.2, 47.6, 38.6, 109.1, 12.6, 31.1, 9.7, 48.8, 33.5, 21.7, 40.6, 51.9, 49.5, 13.0, 0.3, 29.2, 1.8,
           16.3, 33.5, 16.6, 48.9, 42.1, 9.2, 11.4, 14.2, 37.6, 1.2, 3.8, 40.7, 27.9, 12.9, 5.5, 82.1, 15.0,
           1.7, 15.9, 49.9, 9.2, 33.7, 27.8, 36.6, 37.8, 59.6, 49.4, 20.2, 9.9, 9.6, 17.3, 1.0, 11.1) 

## a ####
lambda_est <- 1/mean(datos)
1/lambda_est

## b ####
lim <- 20
exp(-lambda_est*lim)

## c ####
p <- 0.9
-1/lambda_est * log(p)

## d ####
lambda <- 1/25
lim <- 14
lim^2 * exp(-2*lim*lambda) * (lambda)^2 / ( (1-exp(-lim*lambda)) * exp(-lim*lambda) )

# Ejercicio 3 ####
## a ####
var_media = function(nrep, mu, sigma, n, gl, seed = 1){
  valores_estimador <- c()
  set.seed(seed)
  for(i in 1:nrep){
    datos <- mu + sigma*rt(n = n, df = gl)
    valores_estimador <- c(valores_estimador,mean(datos))
  }
  return(var(valores_estimador))
}

var_mediana = function(nrep, mu, sigma, n, gl, seed = 1){
  valores_estimador <- c()
  set.seed(seed)
  for(i in 1:nrep){
    datos <- mu+sigma*rt(n = n, df = gl)
    valores_estimador <- c(valores_estimador,median(datos))
  }
  return(var(valores_estimador))
}

var_media_podada = function(nrep, mu, sigma, n, gl, seed = 1){
  valores_estimador <- c()
  set.seed(seed)
  for(i in 1:nrep){
    datos <- mu+sigma*rt(n = n, df = gl)
    valores_estimador <- c(valores_estimador, mean(datos[quantile(datos,0.1)<datos & quantile(datos,0.9)>datos]))
  }
  return(var(valores_estimador))
}

# n=10 , grados de libertad = 4 , mu=10 , sigma = 1
var_media(10000,10,1,10,4)

var_mediana(10000,10,1,10,4)

var_media_podada(10000,10,1,10,4)

eficiencia_mediana_vs_media <- var_media(1000,10,1,10,4)/var_mediana(1000,10,1,10,4)
eficiencia_mediana_vs_media
eficiencia_media_podada_vs_media <- var_media(1000,10,1,10,4)/var_media_podada(1000,10,1,10,4)
eficiencia_media_podada_vs_media

# b ####
eficiencias <- tibble(k=3:10) %>%
  rowwise() %>%
  mutate(mediana = var_media(1000,0,1,10,k)/var_mediana(1000,0,1,10,k),
         media_podada =var_media(1000,0,1,10,k)/var_media_podada(1000,0,1,10,k)) %>%
  pivot_longer(cols = !k, names_to = "Estimador", values_to = "Eficiencia")

eficiencias %>% ggplot(aes(x = k,
                           y = Eficiencia,
                           color = Estimador)) +
  labs(subtitle = "Ambas son la eficiencia de la media sobre la que se muestra. \nEs decir, si el valor es mayor a 1 significa que la media es un estimador más eficiente") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point() +
  geom_line(alpha = .5) +
  theme_bw() +
  theme(legend.position = "top")
