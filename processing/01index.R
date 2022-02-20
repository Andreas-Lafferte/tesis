# Code 8: Corporativism index/measure


# 1. Packages -------------------------------------------------------------

if(!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, sjmisc, summarytools, sjPlot, psych, 
               lavaan, sjlabelled)


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


## 4.1. Correlation matrix (Pearson - Polychoric) ----

# Pearson (inlcude UD)

matriz <- ictwss %>% select(COORD, TYPE, LEVEL, EXT, UD) %>% na.omit() %>% as.data.frame()
matriz <- as_numeric(matriz)

cor(matriz, use = "everything", method = "pearson")

psych::alpha(matriz) # Cronbach's


# Polychoric 

matriz_2 <- ictwss %>% select(COORD, TYPE, LEVEL, EXT) %>% na.omit() %>% as.data.frame()
matriz_2 <- as_numeric(matriz_2)
matriz_2$TYPE <- car::recode(matriz_2$TYPE, recodes = c(" 6 = 5")) # with this re codification, the cor increase 
matriz_2 <- sjlabelled::remove_all_labels(matriz_2)

matriz_poly <- psych::polychoric(matriz_2)

psych::alpha(matriz_poly$rho) # Alpha ordinal

## 4.2. Factor analysis ----

sjPlot::plot_frq(matriz_2$COORD, type = "h", show.mean = TRUE, show.mean.val = TRUE,normal.curve = TRUE, show.sd = TRUE, normal.curve.color = "blue")

sjPlot::plot_frq(matriz_2$TYPE, type = "h", show.mean = TRUE, show.mean.val = TRUE,normal.curve = TRUE, show.sd = TRUE, normal.curve.color = "blue")

sjPlot::plot_frq(matriz_2$LEVEL, type = "h", show.mean = TRUE, show.mean.val = TRUE,normal.curve = TRUE, show.sd = TRUE, normal.curve.color = "blue")

sjPlot::plot_frq(matriz_2$EXT, type = "h", show.mean = TRUE, show.mean.val = TRUE,normal.curve = TRUE, show.sd = TRUE, normal.curve.color = "blue")

psych::KMO(matriz_2)

psych::cortest.bartlett(matriz_2)

scree(matriz_2)

lillie.test(matriz_2$COORD)

det(matriz_2)
