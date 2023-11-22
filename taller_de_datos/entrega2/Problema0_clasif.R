rm(list=ls())

data_normales <- read.table("Distrofia-Data_normales.txt")
data_portadoras <- read.table("Distrofia-Data_portadoras.txt")

portadora <- c(rep(0,nrow(data_normales)),rep(1,nrow(data_portadoras)))


data <-cbind(rbind(data_normales,data_portadoras),portadora)[,c(6:7,9:14)]
names(data)<-c("edad","mes","año","CK","H","PK","LD","portadora")

boxplot(data$edad ~ data$portadora)
boxplot(data$CK ~ data$portadora)$out
boxplot(data$H ~ data$portadora)$out
boxplot(data$PK ~ data$portadora)$out
boxplot(data$LD ~ data$portadora)$out

data[data$PK==-9999,]
data[data$LD==-9999,]

#saco filas correspondientes a obs que no tienen sentido
data_limpia <- data[!(data$PK==-9999) & !(data$LD==-9999),]

boxplot(data_limpia$edad ~ data_limpia$portadora)

boxplot(data_limpia$CK ~ data_limpia$portadora)
boxplot(data_limpia$H ~ data_limpia$portadora)
boxplot(data_limpia$PK ~ data_limpia$portadora)
boxplot(data_limpia$LD ~ data_limpia$portadora)

par(mfrow=c(1,2))
hist(data_limpia$CK[data_limpia$portadora==0],xlim =
                                c(min(data_limpia$CK),max(data_limpia$CK)))
hist(data_limpia$CK[data_limpia$portadora==1],xlim =
                                c(min(data_limpia$CK),max(data_limpia$CK)))

hist(data_limpia$H[data_limpia$portadora==0],xlim =
                        c(min(data_limpia$H),max(data_limpia$H)))
hist(data_limpia$H[data_limpia$portadora==1],xlim =
                        c(min(data_limpia$H),max(data_limpia$H)))

hist(data_limpia$PK[data_limpia$portadora==0],xlim =
                  c(min(data_limpia$PK),max(data_limpia$PK)))
hist(data_limpia$PK[data_limpia$portadora==1],xlim =
                  c(min(data_limpia$PK),max(data_limpia$PK)))

hist(data_limpia$LD[data_limpia$portadora==0],xlim =
                   c(min(data_limpia$LD),max(data_limpia$LD)))
hist(data_limpia$LD[data_limpia$portadora==1],xlim =
                   c(min(data_limpia$LD),max(data_limpia$LD)))


