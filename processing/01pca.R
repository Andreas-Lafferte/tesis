# 1. Packages ----------------------------------------------------------------

if(!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, sjmisc, summarytools, sjPlot, psych, 
               lavaan, sjlabelled, polycor, corrplot, parameters,
               cluster, factoextra, NbClust, stats, FactoMineR)

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

df <- ictwss %>% select(COUNTRY, 3:7) %>% na.omit()
matriz <- ictwss %>% select(3:7) %>% na.omit() 
matriz <- as_numeric(matriz)
matriz <- scale(matriz) %>% as.data.frame()


## PCA ----

psych::KMO(matriz)

scree(matriz)

results_factors <- parameters::n_factors(matriz, type = "PCA", rotation = "none", algorithm = "minres", n_max = NULL)
plot(results_factors)
as.data.frame(results_factors)

# prcomp
respca <- prcomp(x = matriz[,c(1,2,3,5)])

respca$rotation
summary(respca)
respca$sdev * respca$sdev

cor(matriz[,c(1,2,3,5)], respca$x[,1:2])

screeplot(respca, type = "l")
abline(1,0, col = "red", lty = 2)


# psych
pca2 <- principal(r = matriz[,c(1,2,3,4,5)], nfactors = 1, rotate = "oblimin", scores = T)
pca2$scores
pca2$loadings
pca2$values

cor(matriz, pca2$scores)

# factoMiner
pca3 <- PCA(X = matriz, ncp = 2, graph = T)



# visualization

fviz_eig(respca)

fviz_pca_ind(respca)

fviz_pca_ind(respca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE     # Avoid text overlapping
)


fviz_contrib(respca,choice = "var")
fviz_contrib(respca,choice = "ind")

fviz_pca_biplot(respca, repel = FALSE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)


## HACER UN PCA CON 1 SOLO COMPONENTE Y GRAFICARLO, UTILIZANDO LAS 5 VARIABLES
## REPETIR LO MISMO PERO CON LAS 4 VARIABLES (MENOS EXT) 

## PARA COMPROBAR, HACER DOS PCA: UNO CON EXT Y OTRO SIN EXT Y VER SI LOS PAISES SE UBICAN SIMILAR


## CONCLUSION: COMPARANDO UN PCA SIN EXT Y EL K MEANS SIN EXT, LOS PAISES SE UBICAN/PUNTUAN IGUALES. DE HECHO
## AMBAS TECNICAS SON MUY PARECIDAS EN ESTE SENTIDO PUES EL PC1 Y PC2 EXPLICAN LA MISMA CANTIDAD DE VARIANZA EN EL PCA COMO EN EL K MEANS (72,9 Y 14,5 RESPECTIVAMENTE)

## HABIENDO UTILIZADO 3 TECNICAS DIFERENTES (EFA, PCA Y K MEANS), LOS RESULTADOS SON BASTANTE SIMILARES Y UBICAN A LOS PIASES EN LOS MISMOS NIVELES DE CORPORATIVISMO
## AHORA RESTA SABER CUAL SERÍA LA MEJOR FORMA (JC) Y PREGUNTARSE POR EL ROL DE LA UD EN EL INDICE