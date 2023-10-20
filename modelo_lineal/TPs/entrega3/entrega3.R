pacman::p_load(here, tidyverse, MASS)

esperanza2015 <- read.table(here("modelo_lineal/TPs/entrega3/data/esperanza2015.txt"))

#Ejercicio 1
#income, child, energy, gini, school, hdi
plot(esperanza2015$income,esperanza2015$life)
plot(esperanza2015$child,esperanza2015$life)
plot(esperanza2015$energy,esperanza2015$life)
plot(esperanza2015$gini,esperanza2015$life)
plot(esperanza2015$school,esperanza2015$life)
plot(esperanza2015$hdi,esperanza2015$life)

boxplot(esperanza2015$life ~ esperanza2015$status)

round(cor(esperanza2015[,c(2:9)],use="pairwise.complete.obs"), 3)
#life vs income: 0.6612151
#life vs child: -0.8685162
#life vs dtp3: 0.5536837
#life vs school: 0.6929505

#Ejercicio 2
ajuste<-lm(life~ child, data = esperanza2015)
summary(ajuste)


pred<-ajuste$fitted.values
residuos<-ajuste$residuals
plot(pred,residuos)
plot(ajuste)

#Ejercicio 3
ajuste2<- lm(life ~ child + I(child^2),data = esperanza2015)
plot(ajuste2$fitted.values,ajuste2$residuals)
resumen2<- summary(ajuste2)
resumen2$r.squared

ajuste2bis<- lm(life ~ poly(child,2),data = esperanza2015)
plot(ajuste2bis$fitted.values,ajuste2bis$residuals)
resumen2bis<- summary(ajuste2bis)
resumen2bis$r.squared

resumen2$sigma
resumen2bis$sigma

#pruebo R^2 a mano
ssr<-sum((ajuste2$fitted.values-mean(esperanza2015$life,na.rm=TRUE))^2,na.rm=TRUE)
ssto<-sum((esperanza2015$life-mean(esperanza2015$life,na.rm=TRUE))^2,na.rm=TRUE)
ssr/ssto
###

library(rms)
vif(ajuste2)
vif(ajuste2bis)

cor(esperanza2015$child,esperanza2015$child^2)
pol<-poly(esperanza2015$child,2)
cor(pol[,1],pol[,2])


#Ejercicio 4
child.cen<-scale(esperanza2015$child)
esperanza2015$child.cen<-child.cen
ajuste3<- lm(life ~ child.cen + I(child.cen^2),data = esperanza2015)
resumen3<- summary(ajuste3)
resumen3$r.squared
resumen3$sigma
vif(ajuste3)

#Ejercicio 5
predict(ajuste2,newdata=data.frame(child=c(10,50,100,120)))
predict(ajuste2bis,newdata=data.frame(child=c(10,50,100,120)))
predict(ajuste3,newdata=data.frame(child=c(10,50,100,120)))
data.pred=(c(10,50,100,120)-mean(esperanza2015$child))/sd(esperanza2015$child)
coef3<-ajuste3$coefficients
coef3[1]+coef3[2]*data.pred+coef3[3]*data.pred^2


