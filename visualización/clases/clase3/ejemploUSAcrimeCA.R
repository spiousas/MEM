library(factoextra)
# para visualizar usando ggplot

#El set de datos USArrests del paquete b?sico de R contiene el porcentaje de asaltos (Assault),
#asesinatos (Murder) y violaciones (Rape) por cada 100,000 habitantes para cada uno de los
#50 estados de USA (1973).
#Adem?s, tambi?n incluye el porcentaje de la poblaci?n de cada estado que vive en zonas rurales
#(UrbanPoP).


data("USArrests")
head(USArrests)
datos<-USArrests

#El promedio de los datos muestra que hay tres veces m?s violaciones que asesinatos
# 8 veces m?s asaltos que secuestros.

apply(X = datos, MARGIN = 2, FUN = mean)
##   Murder  Assault UrbanPop     Rape 
##    7.788  170.760   65.540   21.232

# La varianza es muy distinta entre las variables, en el caso de Assault, la varianza es varios ?rdenes de 
#magnitud superior al resto.

apply(X = datos, MARGIN = 2, FUN = var)
##     Murder    Assault   UrbanPop       Rape 
##   18.97047 6945.16571  209.51878   87.72916

library(corrplot)
corrplot(cor(datos), order = "hclust")
stars( datos , labels = abbreviate( rownames( datos ), 8), 
       nrow = 5, key.loc = c(2, 13 ), full = TRUE )

#Si no se estandarizan las variables para que tengan media cero y desviaci?n est?ndar 1 antes de realizar el
#estudio PCA, la variable Assault dominar? la mayor?a de las componentes principales.

#como hacerlo a mano

datos.centrados<-scale(datos)#,scale=FALSE) #estandarizados o solo centrados 
matriz.cov <- cov(datos.centrados)
matriz.cov
#otra forma
IRaizvar<-diag(apply(X = datos, MARGIN = 2, FUN = sd)^-1)
IRaizvar%*%cov(datos)%*%IRaizvar

eigen <- eigen(matriz.cov)
autovalores<-eigen$values
autovectores<-eigen$vectors
round(t(autovectores)%*%matriz.cov%*%autovectores,3)

pc.scores <- t(autovectores) %*% t(datos.centrados)
rownames(pc.scores) <- c("PC1", "PC2", "PC3", "PC4")
head(t(pc.scores))


#con esto puedo recuperar todos los datos
datoscentrados.recuperados <- t(autovectores %*% pc.scores)
#chequemos
datoscentrados.recuperados[1,]
datos.centrados[1,]

# usando SVD
datos.svd<-svd(matriz.cov)
names(datos.svd)
datos.svd$d #autovalores
datos.svd$u #autovectores

# USANDO LA FUNCION DE R
#La funci?n prcomp() es una de las m?ltiples funciones en R que realizan PCA.
#Por defecto, prcomp() centra las variables para que tengan media cero, 
#pero si se quiere adem?s que su desviaci?n est?ndar sea de uno,
#hay que indicar scale = TRUE.


res.pca <- prcomp(datos, scale = TRUE)
names(res.pca)

#Los elementos center y scale almacenados en el objeto pca contienen la media y desviaci?n t?pica
#de las variables previa estandarizaci?n (en la escala original).

res.pca$center
res.pca$scale

#rotation contiene el valor de los loadings  para cada componente (eigenvector).
#El n?mero m?ximo de componentes principales se corresponde con el m?nimo(n-1,p), que en este caso es min(49,4)=4.

res.pca$rotation
#son los 
autovectores

#Analizar con detalle el vector de loadings que forma cada componente puede ayudar
#a interpretar que tipo de informaci?n recoge cada una de ellas. 
#Por ejemplo, la primera componente es el resultado de la siguiente 
#combinaci?n lineal de las variables originales:
#  PC1=0.5358995 Murder 0.5831836 Assault? 0.2781909 UrbanPop 0.5434321 Rape
#Los pesos asignados en la primera componente a las variables Assault, Murder y Rape
#son aproximadamente iguales
#entre ellos y bastante superiores al asignado a UrbanPoP, 
#esto significa que la primera componente 
#recoge mayoritariamente la informaci?n correspondiente a los delitos.
#En la segunda componente, es la variable UrbanPoP la que tiene con diferencia mayor peso, por lo que 
#se corresponde principalmente con el nivel de urbanizaci?n del estado. 
#Si bien en este ejemplo la interpretaci?n de las componentes es bastante clara, 
#no en todos los casos ocurre lo mismo.

#La funci?n prcomp() calcula autom?ticamente el valor de las componentes principales
#para cada observaci?n (principal component scores)
#multiplicando los datos por los vectores de loadings. 
#El resultado se almacena en la matriz x.


#sdev	desviaciones est?ndar de los componentes principales sqrt(lambdas )
#rotation	las columnas son los autovectores vectores
#center	media de cada variable
#scale	desvio de cada variable
#X	coordenadas de las observaciones sobre los componentes principales.
# 

head(res.pca$x)
# lo que antes guardamos en t(pc.scores)
head(t(pc.scores))

# ejemplo de dos estados

playa<-datos.centrados[9,] #florida
res.pca$x[9,]
res.pca$rotation[,1] #PC1
playa1<-t(playa)%*% (res.pca$rotation[,1])
playa2<-t(playa)%*% (res.pca$rotation[,2])
playa3<-t(playa)%*% (res.pca$rotation[,3])
playa4<-t(playa)%*% (res.pca$rotation[,4])
playa1<-as.numeric(playa1)
playa2<-as.numeric(playa2)
playa3<-as.numeric(playa3)
playa4<-as.numeric(playa4)

playa1 *res.pca$rotation[,1]+playa2 *res.pca$rotation[,2]+playa3 *res.pca$rotation[,3]+playa4*res.pca$rotation[,4]
playa #dato de florida centrado

macanudo<-datos.centrados[45,] # Vermont
res.pca$x[45,]
macanudo1<-t(macanudo)%*% (res.pca$rotation[,1])
macanudo2<-t(macanudo)%*% (res.pca$rotation[,2])
macanudo3<-t(macanudo)%*% (res.pca$rotation[,3])
macanudo4<-t(macanudo)%*% (res.pca$rotation[,4])
macanudo1<-as.numeric(macanudo1)
macanudo2<-as.numeric(macanudo2)
macanudo3<-as.numeric(macanudo3)
macanudo4<-as.numeric(macanudo4)

macanudo1 *res.pca$rotation[,1]+macanudo2 *res.pca$rotation[,2]+macanudo3 *res.pca$rotation[,3]+macanudo4*res.pca$rotation[,4]
macanudo #dato de Vermont centrado



#Una vez calculadas las componentes principales, se puede conocer la varianza explicada por cada una de ellas,
#la proporci?n respecto al total y la proporci?n de varianza acumulada.


res.pca$sdev^2 # estos son los autovalores
autovalores

res.pca$sdev^2/sum(res.pca$sdev^2)


# Libreria factoextra R
# get_eigenvalue(res.pca): calcula  los valores propios/varianzas de los componentes principales
#fviz_eig(res.pca): Visualiza los valores propios
#get_pca_ind(res.pca), get_pca_var(res.pca): calcula los resultados para individuos y variables.
#fviz_pca_ind(res.pca), fviz_pca_var(res.pca): Visualiza los resultados individuales y variables.
#fviz_pca_biplot(res.pca): Hace el biplot de individuos y variables.


eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca,choice="variance")

# plotea el porcentaje de varianza explicado. sdev al cuadrado

#Nos quedamos con las primeras dos componentes
#PC1 pone el aprox mismo peso en Asalto, Asesinato y Violaci?n,
#con mucho menos peso en UrbanPop. 
#Por lo tanto, este componente corresponde aproximadamente 
#a una medida de las tasas generales de delitos graves.
#PC2 pone la mayor parte de su peso en UrbanPop y mucho menos peso en las otras
#tres caracter?sticas.Por lo tanto, este componente corresponde aproximadamente 
#al nivel de urbanizaci?n del estado.


res.pca$rotation[,1:2]
res.pca$rotation <- -res.pca$rotation
res.pca$x        <- -res.pca$x

#Grafico de individuos
#######################
#El siguiente grafico muestra que 50 estados mapeados
#de acuerdo con los 2 componentes principales. 

get_pca_ind(res.pca)
#$coord coordenadas
head(get_pca_ind(res.pca)$coord)
head(res.pca$x)
head(t(pc.scores)[,1:2])
fviz_pca_ind(res.pca)

#Los grandes puntajes positivos en el primer componente,
#como California, Nevada y Florida, 
#tienen altas tasas de criminalidad, mientras que estados
#como Dakota del Norte, con puntajes negativos en el
#primer componente, tienen bajas tasas de criminalidad.
#California tambi?n tiene un puntaje alto en el segundo
#componente, lo que indica un alto nivel de urbanizaci?n
# mientras que lo contrario es cierto para estados 
#como Mississippi.
#Los estados cercanos a cero en ambos componentes, como
#Indiana, tienen niveles aproximadamente medios de
#delincuencia y urbanizaci?n.

##cos2 coseno al cuadrado entre el individio y cada eje
head(get_pca_ind(res.pca)$cos2)
(res.pca$x[1,1]^2)/(sum(res.pca$x[1,]^2))
(res.pca$x[1,]^2)/(sum(res.pca$x[1,]^2))

#Representar la contribucion de los individuos mediante
#el coseno cuadrado permite parametrizar entre -1 a 1
#el nivel de contribucion de cada individuo a cada
#componente principal.

#$contrib contribuciones
head(get_pca_ind(res.pca)$contrib)
#para mi deberia calcular esto 
(res.pca$x[1,]^2/autovalores[1])


#Para visualizar la contribuci?n de las personas a los dos primeros componentes principales,
fviz_contrib(res.pca, choice = "ind", axes = 1)
sort((res.pca$x[,1])^2) 
# es el mismo orden pero el grafico da porcentajes.
# la linea roja seria 100%/50 total de datos

fviz_contrib(res.pca, choice = "ind", axes = 2)
sort((res.pca$x[,2])^2)


# aca mas produccion, coloreando por la contribucion
#Las ciudades con un perfil similar se agrupan.
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)



#Grafico de variables.
#######################
#La representación de las variables difiere de la gráfica 
#de las observaciones: las observaciones están representadas 
#por sus proyecciones, pero las variables están representadas
#por sus correlaciones

get_pca_var(res.pca)

#$coord: coordenadas de variables para crear un diagrama de dispersión
get_pca_var(res.pca)$coord[,1:2]
sqrt(autovalores[1])*(res.pca$rotation[,1])
sqrt(autovalores[2])*(res.pca$rotation[,2])

#$cor: correlacion entra las variables y las proyeccioes 
# da lo mismo que lo anterior
get_pca_var(res.pca)$cor[,1]
c(cor(datos$Murder,res.pca$x[,1]),cor(datos$Assault,res.pca$x[,1]),cor(datos$Rape,res.pca$x[,1]),cor(datos$UrbanPop,res.pca$x[,1]))

#$cos2: representa la calidad de representación de las variables en el mapa de factores. Se calcula como las coordenadas al cuadrado: var.cos2 = var.coord * var.coord.
get_pca_var(res.pca)$cos2[,1]
get_pca_var(res.pca)$cor[,1]*get_pca_var(res.pca)$cor[,1]

library("corrplot") 
corrplot(get_pca_var(res.pca)$cos2, is.corr=FALSE)


#$contrib: contiene las contribuciones (en porcentaje) de las variables a los componentes principales. La contribución de una variable (var) a un componente principal dado es (en porcentaje) : (var.cos2 * 100) / (cos2 total del componente).
get_pca_var(res.pca)$contrib
100*get_pca_var(res.pca)$cos2[,1]/sum(get_pca_var(res.pca)$cos2[,1])

# COntribucion de variable k a la componente j
fviz_contrib(res.pca, choice = "var", axes = 1)
# son las correlaciones al cuadrado estandarizadas
#La l?nea discontinua roja en el gr?fico anterior indica
#la contribuci?n promedio esperada. 
#Si la contribuci?n de las variables fuera uniforme, 
#el valor esperado ser?a 1/4
# se puede hacer lo mismo con las otras dimensiones


fviz_pca_var(res.pca)

#Las variables positivas correlacionadas apuntan
#al mismo lado de la gr?fica. Las variables negativas
#correlacionadas apuntan a lados opuestos de la gr?fica.

#La distancia entre las variables y el origen mide la calidad
#de la representacion de las variables (mayor cuanto mas proxima
#a la circunferencia o circulo de correlacion,
#La calidad de esta representaci?n se mide por el valor
#al cuadrado del coseno (cos2) del angulo del triangulo formado por
#el punto del origen, la observaci?n y su proyecci?n sobre el componente

#Para una variable dada, la suma del cos2 sobre todos los componentes
#principales es igual a 1,
#y si adem?s la variable es perfectamente representable por solo los dos
#primeros componentes principales, la suma de cos2 sobre estos dos es igual a 1.
#Variables posicionadas cerca del origen puede ser un indicativo de
#que seran necesarios mas de dos componentes principales para su representacion.

#Biplot
############
#La correlaci?n entre una variable y un componente principal (PC)
#se ve  como las coordenadas de la variable en la PC.
#La representaci?n de las variables  no se ve en el  gr?fico de las observaciones. 
#Las observaciones est?n representadas por sus proyecciones, 
#Mientras que las variables est?n representadas por sus correlaciones.
#La distancia entre las variables y el origen mide la calidad de las variables en el mapa de factores. Las variables que están alejadas del origen están bien representadas en el mapa de factores.
# El Biplot junta el grafico de observaciones y el de variables

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)



# Una Aplicacion linda
library(Factoshiny)
PCAshiny(datos)
PCAshiny()


# Usando GGPLOT
library(ggplot2)
library(grid)
library(gridExtra)
pca_out <- as.data.frame(res.pca$x)
head(pca_out)


p<-ggplot(pca_out,aes(x=PC1,y=PC2, label=row.names(datos) ))
p<-p+geom_point()+ geom_text(size=3)
p


pca_out_r <- as.data.frame(res.pca$rotation)
pca_out_r$feature <- row.names(pca_out_r)

pp<-ggplot(pca_out_r,aes(x=PC1,y=PC2,label=feature,color=feature ))
pp<-pp+geom_point()+ geom_text(size=3)
pp


# ACA MINI RESUMEN
#library(stats)

# prcomp()
# princomp()

#library(FactoMineR)

# PCA() -tiene PCA con resultados más detallados. Los valores ausentes se reemplazan por la media de cada columna. Pueden incluirse variables categóricas suplementarias. Estandariza automáticamente los datos.

#library(factoextra)

# get_pca()  Extrae la información sobre las observaciones y variables de un análisis PCA.
# get_pca_var() Extrae la información sobre las variables.
# get_pca_ind()  Extrae la información sobre las observaciones.

#Visualizaciones:
# library(FactoMineR)

# fviz_pca_ind() Representación de observaciones sobre componentes principales.
# fviz_pca_var() Representación de variables sobre componentes principales.
# fviz_contrib() Representa la contribución de filas/columnas de los resultados de un pca.
# fviz_screeplot() -> Representación (gráfico barras) de eigenvalores.

# aca hay una pagina bastante completa con lo que se puede hacer
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
