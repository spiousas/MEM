#Librerias
#stats: contiene las funciones dist() para calcular matrices de distancias,kmeans(), hclust(), cuttree() para crear los clusters y plot.hclust() para visualizar los resultados.
#cluster, mclust: contienen m?ltiples algoritmos de clustering y m?tricas para evaluarlos.
#factoextra: extensi?n basada en ggplot2 para crear visualizaciones de los resultados de clustering y su evaluaci?n.
#dendextend: extensi?n para la customizaci?n de dendrogramas.

# datos USArrest
#El set de datos USArrests del paquete b?sico de R contiene el porcentaje de asaltos (Assault),
#asesinatos (Murder) y violaciones (Rape) por cada 100,000 habitantes para cada uno de los
#50 estados de USA (1973).
#Adem?s, tambi?n incluye el porcentaje de la poblaci?n de cada estado que vive en zonas rurales
#(UrbanPoP).

data("USArrests")
head(USArrests)

#datos <- scale(USArrests, center = TRUE, scale = TRUE)

datos<-USArrests
nombres <- rownames(USArrests)
distEuc <- dist(x = datos, method = "euclidean") # es la que da por default

round(as.matrix(distEuc)[1:5, 1:5], 2)

library(factoextra)
distPe <- get_dist(x = datos, method = "pearson")

fviz_dist(dist.obj = distEuc, lab_size = 5) +  theme(legend.position = "none")


heatmap(as.matrix(distEuc), Colv=NA,Rowv=NA, scale='none',symm = T)

#orden de acuerdo al denograma
heatmap(as.matrix(distEuc), Colv=T,Rowv=T, scale='none',symm = T)
#muestra un denograma que usa  complete linkage de default

##############################
# Jerarquicos

#con promedios
hc <- hclust(distEuc, "ave")
plot(hc)
plot(hc, hang = -1,cex=0.8)
groups <- cutree(hc, k=10) 
rect.hclust(hc, k=10, border="red")

fviz_dend(x = hc, k = 5, cex = 0.6) +
  geom_hline(yintercept = 5.5, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia eucl?dea, Average Linkage, K=10")

# CENTROIDE
hc <- hclust(distEuc^2, "cen")
memb <- cutree(hc, k = 10)
plot(hc, hang = -1,cex=0.8)
groups <- cutree(hc, k=10) 
rect.hclust(hc, k=10, border="red")


fviz_dend(x = hc, k = 10, cex = 0.6) +
  geom_hline(yintercept = 5.5, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia eucl?dea, Centroide, K=10")
# WARD
hc <- hclust(distEuc, "ward.D2")
memb <- cutree(hc, k = 4)
plot(hc, hang = -1,cex=0.8)
groups <- cutree(hc, k=4) 
rect.hclust(hc, k=4, border="red")

fviz_dend(x = hc, k = 4, cex = 0.6) +
  geom_hline(yintercept = 5.5, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia eucl?dea, Ward, K=4")

fviz_cluster(object = list(data=datos, cluster=cutree(hc, k=4)),
             ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE,
             labelsize = 8)  +
  labs(title = "Hierarchical clustering + Proyecci?n PCA",
       subtitle = "Distancia eucl?dea, Ward, K=4") +
  theme_bw() +
  theme(legend.position = "bottom")






###########################
# NO JERARQUICOS
#K media

set.seed(123)
km_clusters <- kmeans(x = datos, centers = 4, nstart = 50)
km_clusters 

# Las funciones del paquete factoextra emplean el nombre de las filas del
# dataframe que contiene los datos como identificador de las observaciones.
# Esto permite a?adir labels a los gr?ficos.
# autom?ticamente realiza un PCA y representa las dos primeras componentes principales.
fviz_cluster(object = km_clusters, data = datos, show.clust.cent = TRUE, ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +  theme_bw() +  theme(legend.position = "none")

library(cluster) 
clusplot(datos, km_clusters$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0,col.txt="black",col.p="black", cex.txt=0.8,cex=1.5,title="")

#cuantos clusters
#wss es Elowb
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(datos, method = "euclidean"), nstart = 50)
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "silhouette", k.max = 15) +
  labs(title = "N?mero ?ptimo de clusters")
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "gap_stat", nboot = 500,
             k.max = 15, verbose = FALSE, nstart = 50) +labs(title = "N?mero ?ptimo de clusters")
library(NbClust)# tiene 30 indices
numero_clusters <- NbClust(data = datos, distance = "euclidean", min.nc = 2,
                           max.nc = 10, method = "kmeans", index = "alllong")


#PAM

library(cluster)
fviz_nbclust(x = datos, FUNcluster = pam, method = "wss", k.max = 15,
             diss = dist(datos, method = "manhattan"))
set.seed(123)
pam_clusters <- pam(x = datos, k = 4, metric = "manhattan")
pam_clusters
fviz_cluster(object = pam_clusters, data = datos, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")

# CLARA
clara_clusters <- clara(x = datos, k =4, metric = "manhattan", stand = TRUE,
                        samples = 50, pamLike = TRUE)
fviz_cluster(object = clara_clusters, ellipse.type = "t", geom = "point",
             pointsize = 2.5) +
  theme_bw() +
  labs(title = "Resultados clustering CLARA") +
  theme(legend.position = "none")

#DBSCAN
# para ver mejor como funciona consideremos otros conjunto de datos
data("multishapes")
datos <- multishapes[, 1:2]

plot(datos)
set.seed(321)
km_clusters <- kmeans(x = datos, centers = 5, nstart = 50)
fviz_cluster(object = km_clusters, data = datos, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco") +
  theme_bw() +
  theme(legend.position = "none")

library(fpc)
library(dbscan)

dbscan_cluster <- fpc::dbscan(data = datos, eps = 0.15, MinPts = 5)
fviz_cluster(object = dbscan_cluster, data = datos, stand = FALSE,
             geom = "point", ellipse = FALSE, show.clust.cent = FALSE,
             pallete = "jco") +
  theme_bw() +
  theme(legend.position = "bottom")

# aca un shiny 
# http://langtest.jp/shiny/cluster/
