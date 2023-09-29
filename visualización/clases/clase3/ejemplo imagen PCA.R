setwd("C:/Users/0dani/Dropbox/daniela/docencia/visualizacion/clases/6PCA/img")
library(jpeg)
# Separar la imagen en su tres componentes RGB aplicando
#sobre cada uno el PCA y quedandonos con un solo color
imagen <- readJPEG('imagen-original.jpg')
#array of the dimensions height x width x channels.
color1 <- imagen[,,1]
color2 <- imagen[,,2]
color3 <- imagen[,,3]

rbind(imagen[1:5,1,1],imagen[1:5,1,2],imagen[1:5,1,3])

dim(imagen)
writeJPEG(color1, paste('canal1.jpg', sep = ''))

color1 <- imagen[,,1]

#los otros canales
colorb <- imagen
colorb[,,1:2] <- 0
writeJPEG(colorb, paste('canalb.jpg', sep = ''))
colorg <- imagen
colorg[,,1] <- 0
colorg[,,3] <- 0
writeJPEG(colorg, paste('canalg.jpg', sep = ''))
colorr <- imagen
colorr[,,2:3] <- 0
writeJPEG(colorr, paste('canalr.jpg', sep = ''))


dim(color1)
#Comprobar la imagen
writeJPEG(color1, paste('imagen-blanco-negro.jpg', sep = ''))

# Realizar el análisis de componentes principales
pca <- prcomp(color1, center = FALSE)
# Mostrar la imagen para 15 componentes
n = 15
resultado <- pca$x[,1:n] %*% t(pca$rotation[,1:n]) 
# pca$x tiene (xi,phi1),...(xi,phin) las coordenadas de cada obs en las primeras n componentes
# pca$rotation tiene en cada columna las componentes
writeJPEG(resultado, paste('imagen_comprimida_15_componentes.jpg', sep = ''))


# Mostrar la imagen para 300 componentes
n = 300
resultado <- pca$x[,1:n] %*% t(pca$rotation[,1:n])
writeJPEG(resultado, paste('imagen_comprimida_300_componentes.jpg', sep = ''))


# PCA para imagenes

# Separar la imagen en su tres componentes RGB aplicando sobre cada uno el PCA
imagen.r.pca <- prcomp(imagen[,,1], center = FALSE)
imagen.g.pca <- prcomp(imagen[,,2], center = FALSE)
imagen.b.pca <- prcomp(imagen[,,3], center = FALSE)

# Juntar en una sola variable los tres colores RGB
rgb.pca <- list(imagen.r.pca, imagen.g.pca, imagen.b.pca)

# Generar imagenes con diferentes niveles de componentes principales seleccionados  
componentes.selec <- c(10,15,30,100,200,270,280,300)
componentes.selec.char <- c("010","015","030","100","200","270","280","300")
indice <- 1
sumlamb<-cbind(rep(0,8),rep(0,8),rep(0,8))
for (i in componentes.selec) {
  pca.img <- sapply(rgb.pca, function(j) {compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  writeJPEG(pca.img, paste('comprimida_con_', componentes.selec.char[indice], '_componentes.jpg', sep = ''))
  pca.var.img <- sapply(rgb.pca, function(j) {var.img <- sum(j$sdev[1:i])/sum(j$sdev)}, simplify = 'array')
  sumlamb[indice,]<- t(pca.var.img)
  indice <- indice +1
}


#Cuanto comprime??
original <- file.info('imagen-original.jpg')$size / 1000
ind <- length(componentes.selec)
comparo=cbind(rep(0,8),rep(0,8),rep(0,8),rep(0,8),rep(0,8),rep(0,8))
comparo[,1]=t(componentes.selec)
for (i in 1:ind) {
  full.path <- paste('comprimida_con_', componentes.selec.char[i], '_componentes.jpg', sep = '')
  comparo[i,5]  =file.info(full.path)$size / 1000
  comparo[i,6] =  round(( original-comparo[i,5] ) / original, 2) * 100
  }
comparo[,2:4]<-sumlamb*100
comparo.df=data.frame(round(comparo,3))
names(comparo.df)=c('compon','por.varR','por.varG','por.varB','size.imagen','dif.size.original')
comparo.df


#otra forma de hacer las componentes principales.
#juntando todos los canales en un solo conjunto de datos

imagen.total<- cbind(imagen[,,1],imagen[,,2],imagen[,,3])
total.pca<-prcomp(imagen.total, center = FALSE)

# Generar imagenes con diferentes niveles de componentes principales seleccionados  
componentes.selec <- c(10,15,30,100,200,270,280,300)
componentes.selec.char <- c("010","015","030","100","200","270","280","300")
sumlambT<-cbind(rep(0,8),rep(0,8),rep(0,8))
pca.imgT.array<-imagen
pca.var.imgT<-rep(0,8)
indice<-1
for (i in componentes.selec) {
  pca.imgT <- total.pca$x[,1:i] %*% t(total.pca$rotation[,1:i])
  pca.imgT.array[,,1]<-pca.imgT[,1:770]
  pca.imgT.array[,,2]<-pca.imgT[,771:1540]
  pca.imgT.array[,,3]<-pca.imgT[,1541:2310]
    writeJPEG(pca.imgT.array, paste('Tcomprimida_con_', componentes.selec.char[indice], '_componentes.jpg', sep = ''))
  pca.var.imgT[indice] <- sum(total.pca$sdev[1:i])/sum(total.pca$sdev)
  indice <- indice +1
}

#Cuanto comprime??
original <- file.info('imagen-original.jpg')$size / 1000
ind <- length(componentes.selec)
comparoT=cbind(rep(0,8),rep(0,8),rep(0,8),rep(0,8))
comparoT[,1]=t(componentes.selec)
for (i in 1:ind) {
  full.path <- paste('Tcomprimida_con_', componentes.selec.char[i], '_componentes.jpg', sep = '')
  comparoT[i,3]  =file.info(full.path)$size / 1000
  comparoT[i,4] =  round(( original-comparoT[i,3] ) / original, 2) * 100
}
comparoT[,2]<-pca.var.imgT*100
comparoT.df=data.frame(round(comparoT,3))
names(comparoT.df)=c('compon','por.var','size.imagen','dif.size.original')
comparoT.df
comparo.df
