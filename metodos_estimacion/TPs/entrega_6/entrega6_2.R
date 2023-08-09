pacman::p_load(tidyverse, here, MASS)

data = scan()
# Acé leo los datos copiándolos del práctico

## Intervalo de confianza de la mediana con Bootstrap no paramétrico
int_mediana_bootu <- function(x, nBootstrap, alpha){
    set.seed(1)
    estim_boot <- rep(0, nBootstrap)
    
    for(i in 1:nBootstrap){
      x_sample <- sample(x, length(x), replace = TRUE)
      estim_boot[i] <- median(x_sample)
    }
    se <- sqrt(mean((estim_boot-mean(estim_boot))^2))
    z <- qnorm(p = alpha/2, 
               mean = 0, 
               sd = 1, 
               lower.tail = FALSE)
    
    ci_inf <- median(x) - (z*se)
    ci_sup <- median(x) + (z*se)
    
    return(c(ci_inf,ci_sup))
  }

int_mediana_bootu(data, 1000,0.05)

## Intervalo de confianza de la mediana con Bootstrap paramétrico
int_mediana_bootp <- function(x, nBootstrap, alpha){
    set.seed(1)
    estim_boot <- rep(0, nBootstrap)
    estim <- fitdistr(x, densfun = "gamma")$estimate
    
    for(i in 1:nBootstrap){
      x_sample <- rgamma(n = length(x), 
                         shape = estim[1], 
                         rate = estim[2])
      estim_boot[i] <- median(x_sample)
    }
    
    se <- sqrt(mean((estim_boot-mean(estim_boot))^2))
    z <- qnorm(p = alpha/2, 
               mean = 0, 
               sd = 1, 
               lower.tail = FALSE)
    
    ci_inf = median(x) - (z*se)
    ci_sup = median(x) + (z*se)
    
    return(c(ci_inf,ci_sup))
}

int_mediana_bootp(data,1000,0.05)

## Intervalo de confianza de la mediana teórico no paramétrico
int_mediana_teonp = function(x, alfa){
  
  densidad_est=density(x)
  estmed <- median(x)
  f=splinefun(densidad_est$x,densidad_est$y)
  f_eval=f(estmed)
  
  se = (1/(2*sqrt(length(x))*f_eval))
  z = qnorm((1-0.95)/2,0,1,lower.tail = FALSE)
  int_inf = median(x) - (z*se)
  int_sup = median(x) + (z*se)
  return(c(int_inf,int_sup))
}
int_mediana_teonp(data, 0.05)

## Intervalo de confianza de la mediana teórico paramétrico

int_mediana_teop = function(x,alfa){
  
  estdatos <- fitdistr(x, densfun = "gamma")
  estshape <- estdatos$estimate[1]
  estrate <- estdatos$estimate[2]
  f_eval=dgamma(x = median(x),shape =estshape,rate=estrate )
  
  se=(1/(2*sqrt(length(x))*f_eval))
  z = qnorm((1-0.95)/2,0,1,lower.tail = FALSE)
  int_inf = median(x) - (z*se)
  int_sup = median(x) + (z*se)
  return(c(int_inf,int_sup))
  
}
int_mediana_teop(data, 0.05)


## Cubrimiento empirico con diferentes N.---------------------------------------

cubrimiento_empirico = function(n_vec,shape,rate,nrep,alfa){
  
 
  tabla = matrix(nrow=length(n_vec),ncol=4)
  mediana=qgamma(0.5, shape = shape,rate = rate)
  
  for (i in 1:length(n_vec)){
    cub_emp1=c()
    cub_emp2=c()
    cub_emp3=c()
    cub_emp4=c()
    
    
    for (j in 1:nrep){
      
      datos=rgamma(n_vec[i],shape=shape,rate=rate)
      
      cub_emp1[j] = int_mediana_bootu(datos,30,alfa)[1] <= mediana  & int_mediana_bootu(datos,30,alfa)[2] >= mediana
      cub_emp2[j] = int_mediana_bootp(datos,30,alfa)[1] <= mediana  & int_mediana_bootp(datos,30,alfa)[2] >= mediana
      cub_emp3[j] = int_mediana_teonp(datos,alfa)[1] <= mediana & int_mediana_teonp(datos,alfa)[2] >= mediana
      cub_emp4[j] = int_mediana_teop(datos,alfa)[1] <= mediana & int_mediana_teop(datos,alfa)[2] >= mediana
      
    }
    
    tabla[i,1]=mean(cub_emp1)
    tabla[i,2]=mean(cub_emp2)
    tabla[i,3]=mean(cub_emp3)
    tabla[i,4]=mean(cub_emp4)
    
  }
  return(tabla)

}
n_vec=c(8,30,100)
cubrimiento_empirico(n_vec,20,4,50,0.05)
