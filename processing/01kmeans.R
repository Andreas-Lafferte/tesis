

# 1. Packages ----------------------------------------------------------------


if(!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, sjmisc, summarytools, sjPlot, psych, 
               lavaan, sjlabelled, polycor, corrplot, parameters,
               cluster, factoextra, NbClust)

options(scipen=999)

# 2. Data -----------------------------------------------------------------

load(file = "../output/data/ictwss.RData")

names(ictwss)
sapply(ictwss, class)

# 3. Recode -------------------------------------------------------------

levels(ictwss$COORD)
levels(ictwss$TYPE) # reco
levels(ictwss$LEVEL)
levels(ictwss$EXT)# reco

label_ictwss <- ictwss

ictwss$TYPE <- car::recode(ictwss$TYPE, recodes = c("0 = 1; 1 = 2; 2 = 3; 3 = 4; 4 = 5; 5 = 6; 6 = 7"), as.factor = T)

table(ictwss$TYPE, label_ictwss$TYPE)

ictwss$EXT <- car::recode(ictwss$EXT, recodes = c("0 = 1; 1= 2; 2 = 3; 3 = 4"), as.factor = T)

table(ictwss$EXT, label_ictwss$EXT)

ictwss$TYPE <- sjlabelled::set_labels(ictwss$TYPE, labels = c("No specific mechanism identified" = 1, "Government sets signals (public sector wages, minimum wage)" = 2, 
                                                              "Pattern bargaining" = 3,
                                                              "Intra-associational (informal centralisation)" = 4, "Inter-associational by peak associations" = 5,
                                                              "Government sponsored bargaining (this includes pacts)" = 6, "Government imposed bargaining (incl. Statutory controls in lieu of bargaining)" = 7))


ictwss$EXT <- sjlabelled::set_labels(ictwss$EXT, labels = c("There are neither legal provisions for mandatory extension, nor is there a functional equivalent" = 1,
                                                            "Extension is exceptional, used in some industries only, because of absence of sector agreements, very high thresholds" = 2,
                                                            "Extension is used in many industries, but with thresholds and Ministers candecide not to extend agreements" = 3,
                                                            "Extension is virtually automatic and more or less general (including enlargement)" = 4))



# 4. Analysis -------------------------------------------------------------

df <- ictwss %>% select(COUNTRY, COORD, TYPE, LEVEL, UD) %>% na.omit() %>% as.data.frame()

df <- df %>% mutate_at(vars(2:4), ~ as.numeric(.))

df_label <- df$COUNTRY
df_scale <- scale(df[,2:5])

## 4.1. Matriz distancia ----

matriz_distancia <- get_dist(x = df_scale, method = "euclidean")

fviz_dist(matriz_distancia, gradient = list(low = "blue", mid = "white", high = "red"))

## 4.2. Cantidad de clusters ----

fviz_nbclust(df_scale, kmeans, method = "wss")
fviz_nbclust(df_scale, kmeans, method = "silhouette")
fviz_nbclust(df_scale, kmeans, method = "gap_stat")



nr_clust <- NbClust(df_scale, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "alllong")
fviz_nbclust(nr_clust)

## 4.3. Kmeans -----

k2 <- kmeans(x = df_scale, centers = 3, nstart = 100)
k2

k_cluster <- k2$cluster
rownames(df_scale) <- paste(df$COUNTRY, 1:dim(df)[1], sep = "_")
  
  
  
fviz_cluster(list(data = df_scale, cluster = k_cluster))
fviz_cluster(k2, data = df_scale, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE)
fviz_cluster(k2, data = df_scale, ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())

res2 <- hcut(df_scale, k = 3, stand = TRUE)
fviz_dend(res2, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF"))

## Pasar a base ----

df %>% 
  mutate(cluster = k2$cluster) %>% 
  group_by(cluster, COUNTRY) %>% 
  summarise_all("mean") %>% print(n = nrow(.)) 

df <- ictwss %>% select(COUNTRY, COORD, TYPE, LEVEL, UD) %>% na.omit() %>% as.data.frame()
df
df$clus<-as.factor(k2$cluster)
df

## CONCLUSION
## COMPARANDO ESTE K MEANS, EN DONDE SE REEMPLAZA LA EXT POR EL UD, CON EL ANALISIS FACTORIAL DONDE SE HACE EL MISMO REEMPLAZO
## LOS RESULTADOS COINCIDEN, ES DECIR, TIENDEN A UBICAR/PUNTUAR, RESPECTIVAMENTE, A LOS MISMOS PAISES DE FORMA SIMILAR, GENERANDO 
## UNA DISTINCION GRUESA DE 3 CLASES DE PAISES (CORPORATIVISTAS, MEDIANAMENTE CORPORATIVISTAS, Y NO CORPORATIVISTAS/LIBERALES)

## LOS ALPHAS CALCULADOS PARA UNA MATRIZ DONDE ESTAN COORD, TYPE, LEVEL Y UD, SON ALTOS. ESTO PUEDE SER INDICATIVO 
## DE QUE, INDEPENDIENTE DE LA FORMA EN QUE SE GENERE EL INDICE (FACTORIAL SCORES, K MEANS CLUSTERS O INDICES SUMATIVOS),
## SE ESTARIA LLEGANDO A RESULTADOS SIMILARES, QUE ES FINALMENTE CLASIFICAR A LOS PAISES SEGUN SU GRADO DE CORPORATIVISMO USANDO ESTAS VAR. 


