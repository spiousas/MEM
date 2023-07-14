# UMP Exactos ####
## ├Poblacion normal: Ensayo Unilateral para $\mu$ con $\sigma$ conocido. ####
# lo hacemos para mu mayor en la alternativa
nivel_empirico_unilateral_exacto_dist_normal=function(nrep,n,mu0,sigma,alfa){
  vec_rechazos=c()
  for (i in 1:nrep){
    muestra=rnorm(n,mu0,sigma) 
    estadistico_obs=(sum(muestra)-(n*mu0))/(sqrt(n*sigma^2))
    cuantil=qnorm(p = 1-alfa)
    vec_rechazos=c(vec_rechazos,estadistico_obs>cuantil)
  }
  return(mean(vec_rechazos))
}

nivel_empirico_unilateral_exacto_dist_normal(15000,1000,2,1,0.05)

pot_unilateral_exacto_dist_normal=function(mu,n,mu0,sigma,alfa){
  1-pnorm(qnorm(p = 1-alfa)+sqrt(n)/sigma*(mu0-mu))
}

# Para mu0=2
pot_unilateral_exacto_dist_normal(2,20,2,1,0.05)

# Sampleo con mu0=2
mus <- seq(1, 3, .01)
plot(mus, pot_unilateral_exacto_dist_normal(mus,20,2,1,0.05))



## ├Poblacion Normal: Ensayo Unilateral para $\sigma^2$ con $\mu$  conocido. ####
# lo hacemos para varianza mayor en la alternativa
nivel_empirico_unilateral_exacto_dist_normal_sigma=function(nrep,n,mu0,sigma,alfa){
  vec_rechazos=c()
  for (i in 1:nrep){
    muestra=rnorm(n,mu0,sigma) 
    estadistico_obs=sum((muestra-mu0)^2)/sigma^2
    cuantil=qchisq(p = 1-alfa,df = n)
    vec_rechazos=c(vec_rechazos,estadistico_obs>cuantil)
  }
  return(mean(vec_rechazos))
}

nivel_empirico_unilateral_exacto_dist_normal_sigma(15000,1000,0,2,0.05)

pot_unilateral_exacto_dist_normal_sigma=function(sigma0,n,mu0,sigma,alfa){
  1-pchisq(df = n, q = sigma0/sigma * qchisq(p = 1-alfa,df = n))
}

# Para sigma0=2
pot_unilateral_exacto_dist_normal_sigma(2,20,0,2,0.05)

# Sampleo con sigma0=2
sigmas <- seq(1, 3, .01)
plot(sigmas, pot_unilateral_exacto_dist_normal_sigma(sigmas,20,0,2,0.05))




## ├Poblacion Exponencial: Ensayo Unilateral para $\lambda$ . ####
# lo hacemos para lambda mayor en la alternativa
nivel_empirico_unilateral_exacto_dist_exp=function(nrep,n,lambda0,alfa){
  vec_rechazos=c()
  for (i in 1:nrep){
    muestra=rexp(n,lambda0) 
    estadistico_obs=(sum(muestra))
    cuantil=qgamma(p = alfa,shape = n,rate = lambda0)
    vec_rechazos=c(vec_rechazos,estadistico_obs<cuantil)
  }
  return(mean(vec_rechazos))
}

nivel_empirico_unilateral_exacto_dist_exp(15000,1000,2,0.05)

# La potencia
pot_unilateral_exacto_dist_exp=function(lambda,n,lambda0,alfa){
  pgamma(shape = n, rate = lambda, q = qgamma(p = alfa,shape = n,rate = lambda0))
}

# Para lambda=2
pot_unilateral_exacto_dist_exp(2,20,2,0.05)

# Sampleo con lambda0=2
lambdas <- seq(1, 3, .01)
plot(lambdas, pot_unilateral_exacto_dist_exp(lambdas,30,2,0.05))

## ├Poblacion Gamma: Ensayo Unilateral para $\lambda$ con $v$ conocido . ####

# Poblacion Gamma: Ensayo Unilateral para beta (rate) con alpha (nu, para no confundir shape) conocido 
# lo hacemos para lambda mayor en la alternativa
nivel_empirico_unilateral_exacto_dist_gamma=function(nrep,n,nu0,lambda0,alfa){
  vec_rechazos=c()
  for (i in 1:nrep){
    muestra=rgamma(n,shape = nu0,rate = lambda0) 
    estadistico_obs=(sum(muestra))
    cuantil=qgamma(p = alfa,shape = n * nu0, rate = lambda0)
    vec_rechazos=c(vec_rechazos,estadistico_obs<cuantil)
  }
  return(mean(vec_rechazos))
}

nivel_empirico_unilateral_exacto_dist_gamma(15000,1000,10,2,0.05)

# La potencia
pot_unilateral_exacto_dist_gamma=function(lambda,n,nu0,lambda0,alfa){
  pgamma(shape = n * nu0, rate = lambda, q = qgamma(p = alfa,shape = n * nu0,rate = lambda0))
}

# Para lambda=2
pot_unilateral_exacto_dist_gamma(2,20,10,2,0.05)

# Sampleo con lambda0=2
lambdas <- seq(1, 3, .01)
plot(lambdas, pot_unilateral_exacto_dist_gamma(lambdas,30,10,2,0.05))

## ├Poblacion Uniforme: Ensayo Unilateral para $\theta$ con $U_{[0;\theta]}$. ####

# Uniforme unilateral
# lo hacemos para theta mayor en la alternativa
nivel_empirico_unilateral_exacto_uniforme=function(nrep,n,theta0,alfa){
  vec_rechazos=c()
  for (i in 1:nrep){
    muestra=runif(n,min = 0,max = theta0) 
    estadistico_obs=max(muestra)
    cuantil=theta0*(1-alfa)^(1/n)
    vec_rechazos=c(vec_rechazos,estadistico_obs>cuantil)
  }
  return(mean(vec_rechazos))
}
nivel_empirico_unilateral_exacto_uniforme(5000,1000,2,0.05)

# Acá la potencia se complica porque es una función partida

pot_unilateral_exacto_uniforme=function(theta,n,theta0,alfa){
  pot <- rep(NA, length(theta))
  for (i in 1:length(theta)) {
    if (theta[i]<theta0) {
      pot[i] <- 0
    } else {
      pot[i] <- 1- (1-alfa)*(theta0/theta[i])^n
    }   
  }
  pot
}

# Para theta=2
pot_unilateral_exacto_uniforme(2,20,2,0.05)

# Sampleo con theta=2
thetas <- seq(1, 3, .01)
plot(thetas, pot_unilateral_exacto_uniforme(thetas,20,2,0.05))

## ├Poblacion Pareto: Ensayo Unilateral para $m$ con $\beta$ conocido. ####
## ├Pareto unilateral ####
library(EnvStats)
nivel_empirico_unilateral_exacto_pareto_para_m=function(nrep,n,m0,beta,alfa){
  vec_rechazos=c()
  for (i in 1:nrep){
    muestra=rpareto(n,shape=beta,scale = m0) 
    estadistico_obs=min(muestra)
    cuantil=m0/(alfa^(1/(n*beta)))
    vec_rechazos=c(vec_rechazos,estadistico_obs>cuantil)
  }
  return(mean(vec_rechazos))
}

nivel_empirico_unilateral_exacto_pareto_para_m(5000,1000,2,1,0.05)


# UMP Asintóticos ####
## ├Poblacion Bernoulli: Ensayo Unilateral asintotico para $p$. ####
# Bernoulli asintotico Unilateral
# Lo hacemos para p mayor en la alternativa
nivel_empirico_unilateral_bernoulli=function(nrep,n,p0,alfa){
  vec_rechazos=c()
  for (i in 1:nrep){
    muestra=rbinom(n,size=1,p=p0) 
    estadistico_obs=(sum(muestra)-n*p0)/sqrt(n*(p0*(1-p0)))
    cuantil=qnorm(p = 1-alfa)
    vec_rechazos=c(vec_rechazos,estadistico_obs>cuantil)
  }
  return(mean(vec_rechazos))
}

nivel_empirico_unilateral_bernoulli(10000,2500,0.5,0.05)

# La potencia
pot_unilateral_bernoulli=function(p,n,p0,alfa){
  1-pnorm(qnorm(1-alfa)*sqrt(p0*(1-p0))/sqrt(p*(1-p)) + n/sqrt(n*(p*(1-p))) * (p-p0))
}

# Para p=.5
pot_unilateral_bernoulli(.5,2500,0.5,0.05)

# Sampleo para todos los ps validos
ps <- seq(0,1,.01)
plot(ps, pot_unilateral_bernoulli(ps,20,0.5,0.05))
