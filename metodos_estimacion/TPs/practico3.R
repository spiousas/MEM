# Ejercicio 2 ####
datos <- c(16.2, 47.6, 38.6, 109.1, 12.6, 31.1, 9.7, 48.8, 33.5, 21.7, 40.6, 51.9, 49.5, 13.0, 0.3, 29.2, 1.8,
           16.3, 33.5, 16.6, 48.9, 42.1, 9.2, 11.4, 14.2, 37.6, 1.2, 3.8, 40.7, 27.9, 12.9, 5.5, 82.1, 15.0,
           1.7, 15.9, 49.9, 9.2, 33.7, 27.8, 36.6, 37.8, 59.6, 49.4, 20.2, 9.9, 9.6, 17.3, 1.0, 11.1) 

## a ####
lambda_est <- 1/mean(datos)
1/lambda_est

## b ####
# Esto sale de P(X>20) = 1 - P(X<20) = 1 - (1 - exp(-lambda*x)) = exp(-lambda*x)
lim <- 20
exp(-lambda_est*lim)

## c ####
# Esto sale de P(X>lim) = p -> exp(-lambda*lim) = p -> lim = -1/lambda*log(p)
p <- 0.9
-1/lambda_est * log(p)

## d ####
lambda <- 1/25
lim <- 14

varEMV <- lim^2 * exp(-2*lim*lambda) * (lambda)^2
varNP <- ( (1-exp(-lim*lambda)) * exp(-lim*lambda) )

eff <- varEMV/varNP
eff

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

# Ejercicio 4 ####
pacman::p_load(MASS, here)

# Estos salen de la práctica 2
alfa_MO_gamma <- function(datos){
  estimacion <- (mean(datos))^2/(mean(datos^2)-(mean(datos))^2)
  return(estimacion)
}

lambda_MO_gamma <- function(datos){
  estimacion <- mean(datos)/(mean(datos^2)-(mean(datos))^2)
  return(estimacion)
}

# leo y junto todos los archivos
Illinois60 <- read.table(here("metodos_estimacion/TPs/Data_Illinois/illinois60.txt"))
Illinois61 <- read.table(here("metodos_estimacion/TPs/Data_Illinois/illinois61.txt"))
Illinois62 <- read.table(here("metodos_estimacion/TPs/Data_Illinois/illinois62.txt"))
Illinois63 <- read.table(here("metodos_estimacion/TPs/Data_Illinois/illinois63.txt"))
Illinois64 <- read.table(here("metodos_estimacion/TPs/Data_Illinois/illinois64.txt"))

data <- rbind(Illinois60, Illinois61, Illinois62, Illinois63, Illinois64) %>% pull(V1)

emo_alfa<-alfa_MO_gamma(data)
emo_lambda<-lambda_MO_gamma(data)
# El EMV lo hago con fitdistr
emv_gamma<-fitdistr(data, densfun = "gamma")

B <-1000
sn<-length(data)
est_alfa_mv <- est_lambda_mv <- 0
est_alfa_mo <- est_lambda_mo <- 0

for(i in 1:B){
  # Creo las dos simulaciones utilizando los parámetros de Mo y de MV
  lluviasboot_mv <-
    rgamma(n, emv_gamma$estimate[1],
           emv_gamma$estimate[2])
  lluviasboot_mo <-
    rgamma(n, emo_alfa,
           emo_lambda)
  emvboot <- fitdistr(lluviasboot_mv, densfun = "gamma")
  est_alfa_mo[i] <- alfa_MO_gamma(lluviasboot_mo)
  est_lambda_mo[i] <- lambda_MO_gamma(lluviasboot_mo)
  est_alfa_mv[i] <- emvboot$estimate[1]
  est_lambda_mv[i] <- emvboot$estimate[2]
}

# El error estándar es la raiz cuadrada de la varianza del estimador
bootalfa_mv <- sqrt(var(est_alfa_mv))
bootlambda_mv <- sqrt(var(est_lambda_mv))

bootalfa_mo <- sqrt(var(est_alfa_mo))
bootlambda_mo <- sqrt(var(est_lambda_mo))

# Las eficiencias son el cociente de las varianzas
eff_alfa<-var(est_alfa_mv)/var(est_alfa_mo)
eff_lambda<-var(est_lambda_mv)/var(est_lambda_mo)

# EXTRA - El código de Marina V. ####

## Analizamos los datos del ingreso total familiar en Argentina en 2019 ####
ingresos <- read.table(here("metodos_estimacion/TPs/Data_ingresos/ingresos_argentina_2019.txt"))
ingresos_pos <- ingresos[ingresos>0]

library(MASS)
emvlnorm <- fitdistr(ingresos_pos, densfun = "lognormal")
emvlnorm

par(mfrow = c(1,2))
hist(ingresos_pos, freq=FALSE, main = "Ingreso total familar")
lines(density(ingresos_pos))
curve(dlnorm(x, meanlog = emvlnorm$estimate[1], sdlog = emvlnorm$estimate[2]), add=TRUE, col = 3)
hist(ingresos_pos, prob = TRUE, main = "Ingreso total familar(zoom)", xlim = c(0,300000), breaks =100)
lines(density(ingresos_pos))
curve(dlnorm(x, meanlog = emvlnorm$estimate[1], sdlog = emvlnorm$estimate[2]), add=TRUE, col = 3)

## Estimamos la varianza y el desvío estandar de los estimadores por el método bootsrap paramétrico asumiendo el modelo lognormal ####
B <-1000
n<-length(ingresos_pos)
estmu <- estsigma <- 0
for(i in 1:B){
  ingresosboot <-
    rlnorm(n, meanlog = emvlnorm$estimate[1], sdlog = emvlnorm$estimate[2])
  emvboot <- fitdistr(ingresosboot, densfun = "lognormal")
  estmu[i] <- emvboot$estimate[1]
  estsigma[i] <- emvboot$estimate[2]
}

vbootmu <- mean((estmu - mean(estmu))^2)
vbootsigma <- mean((estsigma - mean(estsigma))^2)

vbootmu
vbootsigma
sqrt(vbootmu)
sqrt(vbootsigma)

## Estimación de de la mediana y la media sin asumir ninguna distribución ####
mean(ingresos_pos)
median(ingresos_pos)

## Estimación de de la mediana y la media asumiendo un modelo lognormal ####
# Media de la dist lognormal exp(mu)
exp(emvlnorm$estimate[1])
# Mediana de la dist lognormal exp(mu + sigma^2/2)
exp(emvlnorm$estimate[1] + emvlnorm$estimate[2]^2/2)

## Estimación del error estandar de la mediana y la media asumiendo un modelo lognormal ####
B <- 1000
n <- length(ingresos_pos)
estmed_p <- estmed_np <- estmedia_p <- estmedia_np <- 0
for(i in 1:B){
  ingresosboot <-
    rlnorm(n, meanlog = emvlnorm$estimate[1], sdlog = emvlnorm$estimate[2])
  emvboot <- fitdistr(ingresosboot, densfun = "lognormal")
  estmu <- emvboot$estimate[1]
  estsigma <- emvboot$estimate[2]
  estmed_p[i] <- exp(estmu)
  estmed_np[i] <- median(ingresosboot)
  estmedia_p[i] <- exp(estmu + estsigma^2/2)
  estmedia_np[i] <- mean(ingresosboot)
}

sebootmed_p <- sqrt(mean((estmed_p-mean(estmed_p))^2))
sebootmed_np <- sqrt(mean((estmed_np-mean(estmed_np))^2))
sebootmedia_p <-sqrt(mean((estmedia_p-mean(estmedia_p))^2))
sebootmedia_np <- sqrt(mean((estmedia_np-mean(estmedia_np))^2))

sebootmed_p
sebootmed_np
sebootmedia_p
sebootmedia_np

## Eficiencia de los estimadores no paramétricos de la media y la mediana con respecto a MV asumiendo un modelo lognormal ####
sebootmed_p^2/sebootmed_np^2
sebootmedia_p^2/sebootmedia_np^2

## Estimacion de el error estandar por metodo delta ####
sqrt(exp(2*emvlnorm$estimate[1])*emvlnorm$estimate[2]^2/n)
sebootmed_p