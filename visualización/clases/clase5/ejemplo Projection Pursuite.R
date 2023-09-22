#EJEMPLO CON IRIS DATA
# Obtenemos las direcciones

library(Pursuit)
datos <- iris[,1:4]


library(utils)
dir<-getwd()
write.table(iris, file = "iris.csv", sep = ",",
            qmethod = "double")

#"lda" - LDA index
#"pda" - PDA index
#"lr" - Lr index
#"holes" - Holes index (default),
#"cm" - Central Mass index
#"pca" - PCA index
#"friedmantukey" - Friedman Tukey index,
#"entropy" - Entropy index
#"legendre" - Legendre index
# laguerrefourier" - Laguerre Fourier index,
#"hermite" - Hermite index
#PP_Index 
#"naturalhermite" - Natural Hermite index
#"kurtosismax" - Maximum kurtosis index
#"kurtosismin" - Minimum kurtosis index
#"moment" - Moment index
#"mf" - MF index
#"chi" - Chi-square index.
class <- NA # vector que tien el nombre de las clase si se trata de clasificacion
Fcindex <- "Holes" # index function
Dim <- 2 # dimension of data projection
sphere <-  FALSE#TRUE si los datos tienen norma 1

set.seed(123)
Res <- PP_Optimizer(data = datos, class = class, findex = Fcindex,
                    optmethod = "GTSA", dimproj = Dim, sphere = sphere,
                    weight = TRUE, lambda = 0.1, r = 1, cooling = 0.9,
                    eps = 1e-3, maxiter = 1000, half = 30)
names(Res)
print("Indice que use:"); 
Res$findex
print("Proyecciones optimas:"); 
Res$vector.opt

Res1 <- PP_Optimizer(data = datos, class = class, findex = Fcindex,
                    optmethod = "GTSA", dimproj = Dim, sphere = sphere,
                    weight = TRUE, lambda = 0.1, r = 1, cooling = 0.9,
                    eps = 1e-3, maxiter = 1000, half = 30)
print("Indice que use:"); 
Res1$findex
print("Proyecciones optimas:");
Res$vector.opt
Res1$vector.opt

#grafico en cada eje los datos proyectados
plot(Res$proj.data)
plot(Res$proj.data,col=iris$Species)


#con una proyeccion
dim<-1
Fcindex <- "kurtosismax"
Res1 <- PP_Optimizer(data = datos, class = class, findex = Fcindex,
                    optmethod = "GTSA", dimproj = dim, sphere = sphere,
                    weight = TRUE, lambda = 0.1, r = 1, cooling = 0.9,
                    eps = 1e-3, maxiter = 1000, half = 30)
print("Proyecciones optimas:"); Res1$vector.opt
plot(density(Res1$proj.data))

#con una proyeccion
set.seed(999)
dim<-1
Fcindex<- "kurtosismin"
Res2 <- PP_Optimizer(data = datos, class = class, findex = Fcindex,
                     optmethod = "GTSA", dimproj = dim, sphere = sphere,
                     weight = TRUE, lambda = 0.1, r = 1, cooling = 0.9,
                     eps = 1e-3, maxiter = 1000, half = 30)
print("Proyecciones optimas:"); Res2$vector.opt
#plot(Res2$proj.data)
#plot(Res2$proj.data,col=iris$Species)
plot(density(Res2$proj.data))


##############################################################################
#Exploramos con la libreria tour
#Projection pursuite
library(tourr)
#la animacion muestra la densidad en la proyeccion donde las lineas negras
#son las coordenadas en cada componentes.
animate_dist(iris[, 1:4], center = TRUE)
animate(iris[1:4], grand_tour(), display_xy())
# grandteour es una libreria que va seleccionando al azar proyecciones
#proyecta los datos y los grafica

require(colorspace)
pal <- rainbow_hcl(length(levels(iris[,5])))
col <- pal[as.numeric(iris[,5])]
animate_xy(iris[,1:4], col=col)

##############################################
#EJEMPLO WINE
#Este conjunto
#de datos contiene información sobre 178 vinos elaborados a partir
#de tres variedades de uvas, con 13 variables que describen la composición
#física (incluidos los componentes químicos y el color del vino).


library(bootcluster) #aca estan los datos o bien aca https://archive.ics.uci.edu/ml/datasets/wine
data(wine)
library(utils)
dir<-getwd()
write.table(wine, file = "wine.csv", sep = ",",
            qmethod = "double")
names(wine)=c("variedad", "Alcohol","Malic acid","Ash","Alcalinity","Magnesium","phenols","Flavanoids"," Nonflavanoid "," Proanthocyanins","color intensityHue","OD280/OD315","Prolin")
head(wine)
#grafico de proyeccion en una direccion de todas las variables
animate_dist(wine, center = TRUE)

require(colorspace)
pal <- rainbow_hcl(length(levels(as.factor(wine[,1]))))
col <- pal[as.numeric(wine[,1])]
animate_xy(wine[,-1], col=col)
res<-animate_xy(wine[,-1], tour_path=guided_tour(holes()), sphere = TRUE,col=col)


tourGuide3d

#guide tour mezcla grandtour con proyection pursuite
#en lugar de elegir al azar elige las proyecciones segun 
#el proceso de minimizacion de un indice.

