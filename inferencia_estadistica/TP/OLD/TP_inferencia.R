rm(list=ls())

simulacion<-function(Nrep,n,lambda){
  rechazos_T1<-rep(NA,Nrep)
  rechazos_T2<-rep(NA,Nrep)
  for (i in 1:Nrep) {
    muestra<-rexp(n,lambda)
    estadistico_T1<-sum(muestra)
    estadistico_T2<-sqrt(n)*(mean(muestra)-2)/2
    rechazos_T1[i]<-pchisq(estadistico_T1,2*n)<0.05
    rechazos_T2[i]<-pnorm(estadistico_T2,0,1)<0.05
  }
  
  alpha_1<-mean(rechazos_T1)
  alpha_2<-mean(rechazos_T2)
  return(c(alpha_1,alpha_2))
}


Nrep<-10000
n<-30
lambda<-0.5
simulacion(Nrep,n,lambda)

enes<-c(10,30,100,1000)
tabla<-matrix(NA,nrow=2,ncol=4)
colnames(tabla)<-enes
rownames(tabla)<-c("Test 1","Test 2")
for (j in 1:4) {
  tabla[,j]<-simulacion(10000,enes[j],0.5)
}
tabla
