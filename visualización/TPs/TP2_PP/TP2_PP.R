# Ejercicio 2.14 ####
data_abalone <- read_csv(here("visualización/TPs/TP2_CD/data/abalone.csv"), col_names =F)
data_abalone

colnames(data_abalone)=c("Sex","Length","Diameter","Height","Whole_weight","Shucked_weight"
                         ,"Viscera_weight","Shell_weight","Rings")
data_abalone

ggpairs(data_abalone)

# Esto que sigue es usando la librería Tourr
animate_dist(data_abalone[,2:9], center = TRUE)
