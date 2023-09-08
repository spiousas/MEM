rm(list=ls())

#### Ejercicio 1


#### Item A ####
n <- 10
set.seed(78454)
x1 <- runif(n,0,10)
x2 <- runif(n,0,10)

beta0 <- 5
beta1 <- 1
beta2 <- 3
sigma2 <- 1
y <- beta0 + beta1*x1 + beta2*x2 + rnorm(n,0,sqrt(sigma2))

ajuste <- lm(y~x1+x2)
ajuste$coefficients #estimadores de beta
coef(ajuste) #estimadores de beta

######################################
#### MAS COSAS QUE PODESMOS HACER ####
######################################

summary(ajuste)
X <- model.matrix(ajuste) #Obtenemos la matriz X
p <- dim(X)[2]  ## Ojo que no es la cantidad de covariables, es la dimension de la matriz

#IC para cada uno de los betas
alfa <- 0.05
beta1.est <- coef(ajuste)[2]

sqrt(diag(vcov(ajuste))) #forma de obtener los SE de los estimadores de beta
summary(ajuste)$coef[,2] #forma de obtener los SE de los estimadores de beta

SEbeta1 <- sqrt(diag(vcov(ajuste)))[2]
SEbeta1 <- summary(ajuste)$coef[2,2]

c(beta1.est - qt(1-alfa/2 , n-p)*SEbeta1, beta1.est+qt(1-alfa/2 , n-p)*SEbeta1)

## directamente
confint(ajuste)


## estimador de sigma^2
#a mano

sum(ajuste$residuals^2)/(n-p) 
s2 <- summary(ajuste)$sigma^2 


#### Item B ####

Nrep <- 1000
beta1.est <- c()
beta2.est <- c()
s2 <- c()
for (i in 1:Nrep)
{
  y <- beta0+beta1*x1+beta2*x2+rnorm(n,sqrt(sigma2))
  ajuste <- lm(y~x1+x2)
  beta1.est[i] <- ajuste$coefficients[2] # beta1.est[i]=coef(ajuste)[2]
  beta2.est[i] <- ajuste$coefficients[3]
  s2[i] <- sum(ajuste$residuals^2)/(n-p)
}

#### Item C ####
hist(beta1.est,freq=FALSE,main='',xlab = expression(hat(beta)[1]))
varianza_beta1.hat_verdadera <- sigma2*(solve(t(X)%*%X))[2,2]
curve(dnorm(x,mean=beta1,sd=sqrt(varianza_beta1.hat_verdadera)),add=TRUE,col='darkred') ## Ponemos la verdadera esperanza y desvio


#### Item D ####
## Verdadera esperanza del estimador de Beta1.est y varianza de beta1.est
varianza_beta1.hat_verdadera <- sigma2*(solve(t(X)%*%X))[2,2]
desvio_beta1.hat_estimado <- sqrt(diag(vcov(ajuste)))[2]

#### Item E ####
estadistico_T <- (beta1.est-beta1)/sqrt(s2*(solve(t(X)%*%X))[2,2])

#### Item F ####
hist(estadistico_T,freq=FALSE,main='',xlab = expression(T))

#### Item G ####
hist(estadistico_T,freq=FALSE,main='',xlab = expression(T))
curve(dnorm(x,mean=0,sd=1),add=TRUE,col='darkred')

#### Item H ####
curve(dt(x,(n-p)),add=TRUE,col='darkblue')

## Bonus
#################################
hist(s2*(n-p)/sigma2,freq = FALSE,main='',xlab = expression(s^2))
curve(dchisq(x,n-p),add=TRUE,col='darkred')
#################################

#### ITEM I ####
alfa <- 0.1
LI1 <- beta1.est-qt(1-alfa/2,df=n-p)*sqrt(s2*(solve(t(X)%*%X))[2,2])
LS1 <- beta1.est+qt(1-alfa/2,df=n-p)*sqrt(s2*(solve(t(X)%*%X))[2,2])

mean(beta1>LI1 & beta1<LS1)

#### ITEM J ####
LI2 <- beta2.est-qt(1-alfa/2,df=n-p)*sqrt(s2*(solve(t(X)%*%X))[3,3])
LS2 <- beta2.est+qt(1-alfa/2,df=n-p)*sqrt(s2*(solve(t(X)%*%X))[3,3])
mean(beta2>LI2 & beta2<LS2)


#### ITEM K ####
mean(beta2>LI2 & beta2<LS2& beta1>LI1 & beta1<LS1)

#############################################################################
## Ejercicio 2 / Ejercicio4 - P4
setwd("~/Documentos/spiousas")
paralel <- read.csv("paralel")
names(paralel)
paralel <- as.data.frame(paralel)

head(paralel)

n1 <- length(paralel$x1)
n2 <- length(paralel$x2)
y <- c(paralel$y1,paralel$y2)
#fabricamos la matriz de disenio a mano
X <- matrix(0,n1+n2,2)
X[1:n1,1] <- rep(1,n1)
X[(n1+1):(n1+n2),2] <- rep(1,n2)


ajuste <- lm(y~X-1) ## No queremos ajustar con intercept ?que pasa si ajusto con intercept?
summary(ajuste)
p <- dim(X)[2]
n <- length(y)
s2 <- sum(ajuste$residuals^2)/(n-p)
s2 <- summary(ajuste)$sigma^2 
a <- c(1,-1)
#construimos el estadistico del test en este caso
TE <- (t(a)%*%(ajuste$coefficients))/sqrt(s2*t(a)%*%solve(t(X)%*%X)%*%a) ## ver tema vector, matriz, dimension

pvalor <- 2*pt(TE, df=n-p,lower.tail = FALSE)

## Aleternativa

n1 <- length(paralel$x1)
n2 <- length(paralel$x2)
y <- c(paralel$y1,paralel$y2)
#fabricamos la matriz de disenio a mano
X <- matrix(0,n1+n2,1)
X[1:n1,1] <- rep(0,n1)
X[(n1+1):(n1+n2),1] <- rep(1,n2)
ajuste <- lm(y~X) ## No queremos ajustar con intercept ?que pasa si ajusto con intercept?
summary(ajuste)

## Funcion que sirve para testear H_0: a^T beta = c versus H_0: a^T beta != c
test.cl.beta <- function(X,y,a,c)
{
  ajuste <- lm(y~X-1)
  summary(ajuste)
  p <- dim(X)[2]
  n <- length(y)
  s2 <- sum(ajuste$residuals^2)/(n-p)
  
  TE <- (t(a)%*%(ajuste$coefficients)-c)/sqrt(s2*t(a)%*%solve(t(X)%*%X)%*%a)
  
  pvalor <- 2*pt(abs(TE), df=n-p,lower.tail = FALSE)
  pvalor
  
}
test.cl.beta(X,y,c(1,-1),c=0)
t.test(paralel$y1,paralel$y2,var.equal = TRUE)



#### Ejercicio 3 ####
papel <- read.csv("papel")
attach(papel)

names(papel)
n <- dim(papel)[1]
#definimos X
X <- matrix(0,n,3)
X[1:6,1] <- rep(1,6)
X[7:12,2] <- rep(1,6)
X[13:18,3] <- rep(1,6)
y <- resis 
beta.est <- solve(t(X)%*%X)%*%t(X)%*%y #est de mninimos cuadrados a mano

ajuste <- lm(y~X-1) ## De nuevo no debemos incluir intercept

coef(ajuste)
S2 <- summary(ajuste)$sigma^2

#construimos el estadistico a mano
A <- matrix(c(1,0,-1,0,1,-1),2,3,byrow=TRUE)
q <- nrow(A)
num <- t(A%*%beta.est)%*%solve(A%*%solve(t(X)%*%X)%*%t(A))%*%A%*%beta.est
p <- 3
F.obs <- num/(q*S2)
pf(F.obs,q,n-p,lower.tail = FALSE)
## hay que hacerlo a mano cada vez?

ajuste0 <- lm(y~1) ### Aca pienso en vale H0 y todos tienen la misma media asi que Y= mu.global + eps
anova(ajuste0,ajuste) # sirve para comparar modelos 'anidados'

#####################################################################################
####################################################################################
X1 <- rep(0,n)
X1[conc=="B"]  <- 1
X2 <- rep(0,n)
X2[conc=="C"]  <- 1

ajuste2 <- lm(y~X1+X2) ## Aca si debemos incluir intercep pero la interpretacion de los estimadores es distinta
summary(ajuste2)

## uso mas frecuente
conc.f <- as.factor(conc)  # en este caso no hace falta
ajuste3 <- lm(resis~conc.f)
summary(ajuste3)

model.matrix(ajuste3)  # utiliza por defecto la codificacion que definimos antes


a <- contrasts(conc.f) # la funcion contrasts muestra la codificacion

## y si quiero otra categoria como basal?

contrasts(conc.f) <- contr.treatment(3, base = 2) ##Tengo 3 grupos, quiero al grupo 2 como base
summary(lm(resis ~ conc.f))

ajuste4 <- lm(resis ~ 1) ### Aca pienso en vale H0 y todos tienen la misma media asi que Y= mu.global + eps
anova(ajuste4, ajuste3)