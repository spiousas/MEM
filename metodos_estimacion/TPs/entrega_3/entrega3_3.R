pacman::p_load(tidyverse, here)
# Primero las varianzas ####
var_media = function(nrep, mu, sigma, n, gl){
  valores_estimador <- c()
  for(i in 1:nrep){
    set.seed(i)
    datos <- mu + sigma*rt(n = n, df = gl)
    valores_estimador <- c(valores_estimador,mean(datos))
  }
  return(var(valores_estimador))
}

var_mediana = function(nrep, mu, sigma, n, gl){
  valores_estimador <- c()
  for(i in 1:nrep){
    set.seed(i)
    datos <- mu+sigma*rt(n = n, df = gl)
    valores_estimador <- c(valores_estimador,median(datos))
  }
  return(var(valores_estimador))
}

var_media_podada = function(nrep, mu, sigma, n, gl){
  valores_estimador <- c()
  for(i in 1:nrep){
    set.seed(i)
    datos <- mu+sigma*rt(n = n, df = gl)
    valores_estimador <- c(valores_estimador, mean(datos[quantile(datos,0.1)<datos & quantile(datos,0.9)>datos]))
  }
  return(var(valores_estimador))
}

# Ahora calculo las eficiencias y ploteo ####

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

ggsave(here("metodos_estimacion/TPs/entrega_3/spiousas_3_3.pdf"))
  
 