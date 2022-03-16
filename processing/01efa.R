# Code 8: Corporativism index/measure


# 1. Packages -------------------------------------------------------------

if(!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, sjmisc, summarytools, sjPlot, psych, 
               lavaan, sjlabelled, polycor, corrplot, nortest, 
               tseries, coin, parameters, nFactors)

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


## 4.1. Correlation matrix ----

# Pearson (include UD)

matriz_c <- ictwss %>% select(COORD, TYPE, LEVEL, EXT, UD) %>% na.omit() %>% as.data.frame()
matriz_c <- as_numeric(matriz_c)

pearson <- cor(matriz_c, use = "everything", method = "pearson")

psych::alpha(matriz_c) # Cronbach's Alpha = 0.85

hetcor(data = matriz_c, ML = T, use = "complete.obs") # polycor package

corrplot.mixed(pearson)

# Polyserial

matriz_ps <- matriz_c %>% mutate_at(vars(1:4), ~ as.factor(.))

polyserial <- hetcor(data = matriz_ps, ML = F, use = "complete.obs", thresholds = T) # polycor packages

psych::alpha(polyserial$correlations) # Ordinal Alpha = 0.87

corrplot.mixed(polyserial$correlations)

# Polychoric 

### Psych
matriz_po <- ictwss %>% select(COORD, TYPE, LEVEL, EXT) %>% na.omit() %>% as.data.frame()

matriz_po$TYPE <- car::recode(matriz_po$TYPE, recodes = c("6 = 5")) # with this re codification, the cor increase psych

polycorica_1 <- psych::polychoric(matriz_po, ML = F)

psych::alpha(polycorica_1$rho) # Ordinal Alpha = 0.78

corrplot.mixed(polycorica_1$rho)

### Polycor
matriz_po <- ictwss %>% select(COORD, TYPE, LEVEL, EXT) %>% na.omit() %>% as.data.frame()

polycorica_2 <- hetcor(data = matriz_po, ML = F, use = "complete.obs", thresholds = T) # why is different?

psych::alpha(polycorica_2$correlations) # Ordinal Alpha = 0.89

corrplot.mixed(polycorica_2$correlations)

## 4.2. Normality test ----

sjPlot::plot_frq(data = matriz_po, type = "histogram", geom.colors = "#545454", show.mean = T, show.mean.val = T,
                 show.sd = T, normal.curve = T)

boxplot(matriz_c$COORD, col = "white")

stripchart(matriz_c$COORD,    # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 


boxplot(matriz_c$TYPE, col = "white")

stripchart(matriz_c$TYPE,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 


boxplot(matriz_c$LEVEL, col = "white")

stripchart(matriz_c$LEVEL,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 

boxplot(matriz_c$EXT, col = "white")

stripchart(matriz_c$EXT,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 



## Kolmogorov–Smirnov
ks.test(matriz_c$COORD, "pnorm", alternative = "two.sided")
ks.test(matriz_c$LEVEL, "pnorm", alternative = "two.sided")
ks.test(matriz_c$TYPE, "pnorm", alternative = "two.sided")
ks.test(matriz_c$EXT, "pnorm", alternative = "two.sided")

## Lilliefors
lillie.test(matriz_c$COORD)
lillie.test(matriz_c$LEVEL)
lillie.test(matriz_c$TYPE)
lillie.test(matriz_c$EXT)

## Kurtosis and skew

psych::describe(matriz_c[,1:4])

psych::mardia(matriz_c[,1:4])


## Non parametric test Wilcox Signed Rank

# H0 --> the median difference between pairs of observation is zero (normality)
# H1 --> the median difference between pairs of observation is NOT zero (non normality)
# if p < 0.05 = reject H0 (i.e accept H1)

wilcox.test(matriz_c$COORD, matriz_c$TYPE, paired = T) # reject H0 = accept H1
wilcox.test(matriz_c$COORD, matriz_c$LEVEL, paired = T) # non reject H0 = reject H1
wilcox.test(matriz_c$COORD, matriz_c$EXT, paired = T) # non reject H0 = reject H1
wilcox.test(matriz_c$TYPE, matriz_c$LEVEL, paired = T) # reject H0 = accept H1
wilcox.test(matriz_c$TYPE, matriz_c$EXT, paired = T) # non reject H0 = reject H1
wilcox.test(matriz_c$LEVEL, matriz_c$EXT, paired = T) # non reject H0 = reject H1

## Exact p-value
coin::wilcoxsign_test(matriz_po$COORD ~ matriz_po$TYPE, data = matriz_po, distribution = "exact")
coin::wilcoxsign_test(matriz_po$COORD ~ matriz_po$LEVEL, data = matriz_po, distribution = "exact")
coin::wilcoxsign_test(matriz_po$COORD ~ matriz_po$EXT, data = matriz_po, distribution = "exact")
coin::wilcoxsign_test(matriz_po$TYPE ~ matriz_po$LEVEL, data = matriz_po, distribution = "exact")
coin::wilcoxsign_test(matriz_po$TYPE ~ matriz_po$EXT, data = matriz_po, distribution = "exact")
coin::wilcoxsign_test(matriz_po$LEVEL ~ matriz_po$EXT, data = matriz_po, distribution = "exact")


## Linearity: EXT don't contribute much

ggplot(matriz_c, aes(x=COORD, y=TYPE)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_bw()

ggplot(matriz_c, aes(x=COORD, y=LEVEL)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_bw()

ggplot(matriz_c, aes(x=COORD, y=EXT)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_bw()

ggplot(matriz_c, aes(x=TYPE, y=LEVEL)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_bw()

ggplot(matriz_c, aes(x=TYPE, y=EXT)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_bw()

ggplot(matriz_c, aes(x=LEVEL, y=EXT)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_bw()

## 4.3. Test for EFA ----

# determinante
det(polycorica_2$correlations) # Coef = 0.03688264. This value is greater than the necessary value of 0.00001

# KMO
psych::KMO(r = matriz_c[,1:4])

# Test de Bartlett
psych::cortest.bartlett(matriz_c[,1:4]) # p-value significativo

# Scree plot
scree(rx = matriz_c[,1:4])

# Parallel analysis
psych::fa.parallel(x = matriz_c[,1:4], fm = "pa", fa = "fa", nfactors = 1, cor = "poly")

# Others tests
results_factors <- parameters::n_factors(matriz_c[,1:4], type = "FA", rotation = "none", algorithm = "minres", n_max = NULL)
plot(results_factors)
as.data.frame(results_factors)

## 4.4. Factor Analysis ----

psych::fa(r = matriz_c[,1:4], nfactors = 1, rotate = "oblimin", fm = "pa" , cor = "poly") 
# modelo presenta ajuste muy insuficiente RSMR y RSMEA. Tambien pobre varianza explicada (0.39)

## DE SER UN MODELO CON SOLO 1 FACTOR Y OCUPANDO LAS 4 VAR ORIGINALES, ESTE MODELO SERIA EL INDICADO PERO TIENE MAL AJUSTE
psych::fa(r = matriz_c[,1:4], nfactors = 1, rotate = "oblimin", fm = "pa" , cor = "cor") # with cor = pearson the model its better
# con matriz de pearson el modelo mejora levemente en ajuste y cargas, pero no es lo correcto. Varianza explicada sube a (0.63)
# si agregas UD con principal axis y pearson de correlacion, el modelo no mejora y pierde proporcion de varianza

psych::fa(r = matriz_c[,1:4], nfactors = 1, rotate = "oblimin", fm = "gls" , cor = "cor") # gls y pa andan igual en cargas y varianza explicada


psych::fa(r = matriz_c[,1:5], nfactors = 1, rotate = "oblimin", fm = "uls" , cor = "cor") 
# con todos los indicadores el modelo sigue presentando un pobre ajuste pero buenas cargas. Var explicada 0.57

# CON 1 FACTOR EL AJUSTE ES POBRE, TANTO EN UNA MATRIZ CON COORD, TYPE, LEVEL Y EXT, COMO AGREGANDO UD


## PROBAMOS 2 FACTORES

psych::fa(r = matriz_c[,1:5], nfactors = 2, rotate = "oblimin", fm = "uls" , cor = "cor") 
# al aumentar a 2 factores los ajustes son eficientes, pero se genera un segundo factor aparte solo para EXT. Var expliacada 0.74

psych::fa(r = matriz_c[,1:5], nfactors = 2, rotate = "oblimin", fm = "pa" , cor = "cor") 
# con pa ocurre lo mismo pero con mas cargas cruzadas

## CON 2 FACTORES EL AJUSTE ES EFICIENTE, PERO SE GENERA UN SEGUNDO FACTOR DONDE EXT ES QUIEN MAS LOADING TIENE (BAJAS CARGAS CRUZADAS)

# Y si sacamos EXT?
df_fa <- matriz_c[,c(1,2,3,5)]

fa1 <- psych::fa(r = df_fa, nfactors = 1, rotate = "oblimin", fm = "uls" , cor = "cor")
# un modelo de 1 factor sin EXT reemplazandolo por UD, ajusta correcatamente y con una estructura factorial simple. Var 0.65


## MODELO DEFINITIVO HASTA AHORA REEMPLAZANDO EXT POR UD

fa1$scores

psych::alpha(df_fa) # tiene un alpha de cronbach aceptable/alto

